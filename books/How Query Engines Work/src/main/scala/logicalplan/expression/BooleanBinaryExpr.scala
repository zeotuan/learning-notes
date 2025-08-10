package logicalplan.expression

import datatypes.{ArrowTypes, Field}
import logicalplan.LogicalPlan

abstract class BooleanBinaryExpr(
  name: String,
  op: String,
  left: LogicalExpr,
  right: LogicalExpr
) extends BinaryExpr(name, op, left, right) {
  override def toField(input: LogicalPlan): Field = {
    Field(name, ArrowTypes.BooleanType)
  }
}

// Comparison expressions of two logical expressions
case class Eq(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("eq", "=", left, right)
case class Neq(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("neq", "!=", left, right)
case class Gt(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("gt", ">", left, right)
case class Gte(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("gte", ">=", left, right)
case class Lt(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("lt", "<", left, right)
case class Lte(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("lte", "<=", left, right)

// Boolean Logic expressions of two logical expressions
case class And(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("and", "AND", left, right)
case class Or(left: LogicalExpr, right: LogicalExpr) extends BooleanBinaryExpr("or", "OR", left, right)