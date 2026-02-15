package logicalplan.expression

import datatypes.{ArrowTypes, Field}
import logicalplan.LogicalPlan

case class Literal[T](value: T) extends LogicalExpr {
  override def toField(input: LogicalPlan): Field = {
    val fieldType = value match {
      case _: Int => ArrowTypes.Int32Type
      case _: Long => ArrowTypes.Int64Type
      case _: String => ArrowTypes.StringType
      case _: Boolean => ArrowTypes.BooleanType
      case _: Float => ArrowTypes.FloatType
      case _: Double => ArrowTypes.DoubleType
      case _: Short => ArrowTypes.Int16Type
      case _: Byte => ArrowTypes.Int8Type
      case _ => throw new IllegalArgumentException(s"Unsupported literal type: ${value.getClass.getSimpleName}")
    }
    Field(value.toString, fieldType)
  }

  override def toString: String = s"lit($value)"
}

object Literal {
  def lit[T](value: T): Literal[T] = new Literal(value)
}