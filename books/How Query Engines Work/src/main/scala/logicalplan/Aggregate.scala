package logicalplan

import datatypes.Schema
import logicalplan.expression.LogicalExpr

case class Aggregate(
  input: LogicalPlan,
  groupExpr: Seq[LogicalExpr],
  aggExpr: Seq[LogicalExpr]
) extends LogicalPlan {
  override def schema: datatypes.Schema = {
    val groupFields = groupExpr.map(_.toField(input))
    val aggFields = aggExpr.map(_.toField(input))
    Schema(groupFields ++ aggFields)
  }

  override def children: Seq[LogicalPlan] = Seq(input)

  override def toString: String = {
    val groupStr = groupExpr.map(_.toString).mkString(", ")
    val aggStr = aggExpr.map(_.toString).mkString(", ")
    s"Aggregate: groupBy: [$groupStr], aggregations: [$aggStr])"
  }
}
