package logicalplan.expression

import datatypes.Field
import logicalplan.LogicalPlan

/**
 *  LogicalExpr represent an expression in a logical plan for logical query planning.
 *  It provides information needed for planning phase such as field names, types.
 */
trait LogicalExpr {
  /**
   * Returns metadata about the value that will be produced by this expression when evaluated against certain input.
   */
  def toField(input: LogicalPlan): Field

  final def ===(other: LogicalExpr): LogicalExpr = {
    Eq(this, other)
  }

  final def =!=(other: LogicalExpr): LogicalExpr = {
    Neq(this, other)
  }

  final def >(other: LogicalExpr): LogicalExpr = {
    Gt(this, other)
  }

  final def >=(other: LogicalExpr): LogicalExpr = {
    Gte(this, other)
  }

  final def <(other: LogicalExpr): LogicalExpr = {
    Lt(this, other)
  }

  final def <=(other: LogicalExpr): LogicalExpr = {
    Lte(this, other)
  }

  final def &&(other: LogicalExpr): LogicalExpr = {
    And(this, other)
  }

  final def ||(other: LogicalExpr): LogicalExpr = {
    Or(this, other)
  }

  final def +(other: LogicalExpr): LogicalExpr = {
    Add(this, other)
  }

  final def -(other: LogicalExpr): LogicalExpr = {
    Subtract(this, other)
  }

  final def *(other: LogicalExpr): LogicalExpr = {
    Multiply(this, other)
  }

  final def /(other: LogicalExpr): LogicalExpr = {
    Divide(this, other)
  }

  final def %(other: LogicalExpr): LogicalExpr = {
    Modulo(this, other)
  }
}
