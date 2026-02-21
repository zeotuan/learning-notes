package optimizer

import logicalplan.LogicalPlan

trait Optimizer {
  def optimize(logicalPlan: LogicalPlan): LogicalPlan
}

case class BaseOptimizer() extends Optimizer {
  override def optimize(logicalPlan: LogicalPlan): LogicalPlan = {
    val pushDownRule = ProjectionPushdownRule()
    pushDownRule.optimize(logicalPlan)
  }
}

object Optimizer {
  def extractColumns(
    exprs: Seq[logicalplan.expression.LogicalExpr],
    input: LogicalPlan,
  ): Seq[String] = exprs.flatMap {
    case logicalplan.expression.Column(name) => Seq(name)
    case t: logicalplan.expression.BinaryExpr =>
      extractColumns(Seq(t.getRightExpr, t.getLeftExpr), input)
    case t: logicalplan.expression.AggregateExpr =>
      extractColumns(Seq(t.getExpr), input)
    case logicalplan.expression.Literal(_) => Seq.empty
    case _ => throw new IllegalStateException("Unsupported expression type: " + exprs.head.getClass)
  }
}
