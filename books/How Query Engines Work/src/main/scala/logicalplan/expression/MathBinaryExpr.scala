package logicalplan.expression

import datatypes.Field
import logicalplan.LogicalPlan

abstract class MathBinaryExpr(
  name: String,
  op: String,
  left: LogicalExpr,
  right: LogicalExpr
) extends BinaryExpr(name, op, left, right) {
  override def toField(input: LogicalPlan): Field = {
    Field(name, left.toField(input).dataType)
  }
}

case class Add(left: LogicalExpr, right: LogicalExpr) extends MathBinaryExpr("add", "+", left, right)
case class Subtract(left: LogicalExpr, right: LogicalExpr) extends MathBinaryExpr("subtract", "-", left, right)
case class Multiply(left: LogicalExpr, right: LogicalExpr) extends MathBinaryExpr("mult", "*", left, right)
case class Divide(left: LogicalExpr, right: LogicalExpr) extends MathBinaryExpr("div", "/", left, right)
case class Modulo(left: LogicalExpr, right: LogicalExpr) extends MathBinaryExpr("mod", "%", left, right)