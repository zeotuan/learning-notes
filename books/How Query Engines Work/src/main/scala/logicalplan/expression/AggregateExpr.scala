package logicalplan.expression

import datatypes.{ArrowTypes, Field}
import logicalplan.LogicalPlan

abstract class AggregateExpr(name: String, expr: LogicalExpr) extends LogicalExpr {
  override def toField(input: LogicalPlan): Field = {
    Field(name, expr.toField(input).dataType)
  }
  override def toString: String = s"$name(${expr.toString})"
}

case class Sum(input : LogicalExpr) extends AggregateExpr("SUM", input)
case class Avg(input: LogicalExpr) extends AggregateExpr("AVG", input)
case class Min(input: LogicalExpr) extends AggregateExpr("MIN", input)
case class Max(input: LogicalExpr) extends AggregateExpr("MAX", input)
case class Count(input: LogicalExpr) extends AggregateExpr("COUNT", input) {
  override def toField(input: LogicalPlan): Field = Field("COUNT", ArrowTypes.Int32Type)
}
