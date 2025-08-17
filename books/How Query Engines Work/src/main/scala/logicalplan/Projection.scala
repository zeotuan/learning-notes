package logicalplan

import logicalplan.expression.LogicalExpr

case class Projection(
  input: LogicalPlan,
  expr: Seq[LogicalExpr]
) extends LogicalPlan {
  override def schema: datatypes.Schema = {
    val fields = expr.map(_.toField(input))
    datatypes.Schema(fields)
  }

  override def children: Seq[LogicalPlan] = Seq(input)

  override def toString: String = s"Projection(${expr.map(_.toString).mkString(", ")})"
}
