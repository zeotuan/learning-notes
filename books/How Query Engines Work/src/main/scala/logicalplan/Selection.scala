package logicalplan

import logicalplan.expression.LogicalExpr

case class Selection(
  input: LogicalPlan,
  expr: Seq[LogicalExpr]
) extends LogicalPlan {
  override def schema: datatypes.Schema = input.schema

  override def children: Seq[LogicalPlan] = Seq(input)

  override def toString: String = s"Filter: ${expr.map(_.toString).mkString(" AND ")}"
}

object Selection {
  def apply(input: LogicalPlan, expr: LogicalExpr): Selection = {
    Selection(input, Seq(expr))
  }
}
