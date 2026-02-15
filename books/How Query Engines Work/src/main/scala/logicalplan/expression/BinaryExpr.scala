package logicalplan.expression

abstract class BinaryExpr(
  name: String,
  op: String,
  left: LogicalExpr,
  right: LogicalExpr
) extends LogicalExpr {
  override def toString: String = s"$left $op $right"
  def getLeftExpr: LogicalExpr = left
  def getRightExpr: LogicalExpr = right
}