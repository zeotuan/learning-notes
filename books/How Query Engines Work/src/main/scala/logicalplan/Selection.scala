package logicalplan

import logicalplan.expression.LogicalExpr

case class Selection(
  input: LogicalPlan,
  expr: LogicalExpr
) extends LogicalPlan {
  override def schema: datatypes.Schema = input.schema

  override def children: Seq[LogicalPlan] = Seq(input)

  override def toString: String = s"Filter: ${expr}"
}
