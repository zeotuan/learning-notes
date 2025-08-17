package physicalplan.expression

import datatypes.FieldVector.FieldVectorOps
import datatypes.{ArrowFieldVector, ArrowTypes, ColumnVector, FieldVector}
import org.apache.arrow.vector.types.pojo.ArrowType

abstract class MathBinaryExpression(left: Expression, right: Expression) extends BinaryExpression(left, right) {
  override final def evaluateColumnVector(leftResult: ColumnVector, rightResult: ColumnVector): ColumnVector = {
    val valueType = leftResult.getType
    val v = FieldVector(valueType, leftResult.size)
    0 until leftResult.size foreach { i =>
      val leftValue = leftResult.getValue(i)
      val rightValue = rightResult.getValue(i)
      val value = evaluateValue(leftValue, rightValue, valueType)
      v.setValue(i, value)
    }
    v.setValueCount(leftResult.size)
    ArrowFieldVector(v)
  }

  /**
   * Evaluate the expression for the given left and right value of left and right ColumnVector.
   */
  @Override
  def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any
}

case class AddExpression(left: Expression, right: Expression) extends MathBinaryExpression(left, right) {
  override def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] + r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] + r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] + r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] + r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] + r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] + r.asInstanceOf[Double]
      case _ => throw new IllegalArgumentException(s"Unsupported type for addition: $arrowType")
    }
  }
}

case class SubtractExpression(left: Expression, right: Expression) extends MathBinaryExpression(left, right) {
  override def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] - r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] - r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] - r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] - r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] - r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] - r.asInstanceOf[Double]
      case _ => throw new IllegalArgumentException(s"Unsupported type for subtraction: $arrowType")
    }
  }
}

case class MultiplyExpression(left: Expression, right: Expression) extends MathBinaryExpression(left, right) {
  override def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] * r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] * r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] * r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] * r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] * r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] * r.asInstanceOf[Double]
      case _ => throw new IllegalArgumentException(s"Unsupported type for multiplication: $arrowType")
    }
  }
}

case class DivideExpression(left: Expression, right: Expression) extends MathBinaryExpression(left, right) {
  override def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] / r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] / r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] / r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] / r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] / r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] / r.asInstanceOf[Double]
      case _ => throw new IllegalArgumentException(s"Unsupported type for division: $arrowType")
    }
  }
}

case class ModuloExpression(left: Expression, right: Expression) extends MathBinaryExpression(left, right) {
  override def evaluateValue(l: Any, r: Any, arrowType: ArrowType): Any = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] % r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] % r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] % r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] % r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] % r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] % r.asInstanceOf[Double]
      case _ => throw new IllegalArgumentException(s"Unsupported type for modulo: $arrowType")
    }
  }
}
