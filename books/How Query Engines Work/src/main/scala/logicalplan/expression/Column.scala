package logicalplan.expression

import datatypes.Field
import logicalplan.LogicalPlan
/**
 * Represents reference to a named column in a logical plan.
 */
case class Column(name: String) extends LogicalExpr {
  override def toField(input: LogicalPlan): Field = {
    input.schema.fields.find(_.name == name) match {
      case Some(field) => field
      case None => throw new IllegalArgumentException(s"Column '$name' not found in input schema.")
    }
  }

  override def toString: String = s"col($name)"
}
