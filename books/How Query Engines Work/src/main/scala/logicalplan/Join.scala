package logicalplan

import logicalplan.JoinType.JoinType

case class Join(
  left: LogicalPlan,
  right: LogicalPlan,
  joinType: JoinType,
  condition: Option[logicalplan.expression.LogicalExpr]
) extends LogicalPlan {
  override def schema: datatypes.Schema = {
    datatypes.Schema(left.schema.fields ++ right.schema.fields)
  }

  override def children: Seq[LogicalPlan] = Seq(left, right)

  override def toString: String = {
    val condStr = condition.map(_.toString).getOrElse("None")
    s"Join(type: $joinType, condition: $condStr)"
  }

}

object JoinType extends Enumeration {
  type JoinType = Value
  val Inner, LeftOuter, RightOuter, FullOuter, Cross = Value
}
