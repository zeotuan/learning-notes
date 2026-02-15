package queryplanner

import datasource.DataSource
import datatypes.Schema
import logicalplan.LogicalPlan
import logicalplan.expression.LogicalExpr
import physicalplan.aggregate.HashAggregateExec
import physicalplan.{PhysicalPlan, ProjectionExec, ScanExec, SelectionExec}

import java.time.{Instant, LocalDate}

object QueryPlanner {
  def createPhysicalPlan(plan: LogicalPlan): PhysicalPlan = plan match {
    case logicalplan.Scan(_, datasource, projection) => ScanExec(datasource, projection)
    case logicalplan.Projection(input, expr) =>
      val inputPhysicalPlan = createPhysicalPlan(input)
      val inputPhysicalExpr = expr.map(e => createPhysicalExpr(e, input))
      val schema = Schema(expr.map(e => e.toField(input)))
      ProjectionExec(inputPhysicalPlan, inputPhysicalExpr, schema)
    case logicalplan.Selection(input, expr) =>
      val inputPhysicalPlan = createPhysicalPlan(input)
      val inputPhysicalExpr = createPhysicalExpr(expr, input)
      SelectionExec(inputPhysicalPlan, inputPhysicalExpr)
    case logicalplan.Aggregate(input, groupExpr, aggExpr) =>
      val inputPhysicalPlan = createPhysicalPlan(input)
      val groupPhysicalExpr = groupExpr.map(e => createPhysicalExpr(e, input))
      val aggrPhysicalExpr  = aggExpr.map {
        case logicalplan.expression.Max(expr)  => physicalplan.expression.aggregate.MaxAggregateExpression(createPhysicalExpr(expr, input))
        case logicalplan.expression.Min(expr)  => physicalplan.expression.aggregate.MinAggregateExpression(createPhysicalExpr(expr, input))
        case logicalplan.expression.Sum(expr)  => physicalplan.expression.aggregate.SumAggregateExpression(createPhysicalExpr(expr, input))
        case logicalplan.expression.Count(expr)  => physicalplan.expression.aggregate.CountAggregateExpression(createPhysicalExpr(expr, input))
        case logicalplan.expression.Avg(expr)  => physicalplan.expression.aggregate.AvgAggregateExpression(createPhysicalExpr(expr, input))
        case _ => throw new NotImplementedError("Unimplemented aggregate expression")
      }
      new HashAggregateExec(inputPhysicalPlan, groupPhysicalExpr, aggrPhysicalExpr, plan.schema)
  }

  def createPhysicalExpr(logicalExpr: LogicalExpr, input: LogicalPlan): physicalplan.expression.Expression = logicalExpr match {
    case logicalplan.expression.Literal(l: LocalDate) => physicalplan.expression.LiteralExpression(l.toEpochDay)
    case logicalplan.expression.Literal(l: Instant) => physicalplan.expression.LiteralExpression(l.toEpochMilli)
    case logicalplan.expression.Literal(l) => physicalplan.expression.LiteralExpression(l)
    case logicalplan.expression.Column(name) =>
      val i = input.schema.fields.indexWhere(_.name == name)
      if (i == -1)
        throw new IllegalArgumentException(s"Column $name not exist")
      physicalplan.expression.ColumnExpression(i)

    case binExpr: logicalplan.expression.BinaryExpr =>
      val left = createPhysicalExpr(binExpr.getLeftExpr, input)
      val right = createPhysicalExpr(binExpr.getRightExpr, input)
      binExpr match {
        // Math operations
        case logicalplan.expression.Add(_, _) => physicalplan.expression.AddExpression(left, right)
        case logicalplan.expression.Subtract(_, _) => physicalplan.expression.SubtractExpression(left, right)
        case logicalplan.expression.Multiply(_, _) => physicalplan.expression.MultiplyExpression(left, right)
        case logicalplan.expression.Divide(_, _) => physicalplan.expression.DivideExpression(left, right)
        case logicalplan.expression.Modulo(_, _) => physicalplan.expression.ModuloExpression(left, right)

        // Comparison operations
        case logicalplan.expression.Eq(_, _) => physicalplan.expression.EqExpression(left, right)
        case logicalplan.expression.Neq(_, _) => physicalplan.expression.NeqExpression(left, right)
        case logicalplan.expression.Gt(_, _) => physicalplan.expression.GtExpression(left, right)
        case logicalplan.expression.Gte(_, _) => physicalplan.expression.GteExpression(left, right)
        case logicalplan.expression.Lt(_, _) => physicalplan.expression.LtExpression(left, right)
        case logicalplan.expression.Lte(_, _) => physicalplan.expression.LteExpression(left, right)
        case _ => throw new NotImplementedError("Unimplemented binary expression")
      }
  }

}
