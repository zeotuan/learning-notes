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
}
