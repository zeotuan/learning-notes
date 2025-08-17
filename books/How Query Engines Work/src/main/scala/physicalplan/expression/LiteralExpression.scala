package physicalplan.expression

import datatypes.{ArrowTypes, ColumnVector, LiteralColumnVector, RecordBatch}

class LiteralExpression[T](value: T) extends Expression {
  override def evaluate(input: RecordBatch): ColumnVector = {
    val valueType = value match {
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
    LiteralColumnVector(valueType, value, input.rowCount)
  }
}
