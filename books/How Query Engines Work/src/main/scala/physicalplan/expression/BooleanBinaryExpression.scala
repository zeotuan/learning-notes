package physicalplan.expression

import datatypes.{ArrowFieldVector, ArrowTypes, ColumnVector}
import org.apache.arrow.memory.RootAllocator
import org.apache.arrow.vector.BitVector
import org.apache.arrow.vector.types.pojo.ArrowType

abstract class BooleanBinaryExpression(
  left: Expression,
  right: Expression
) extends BinaryExpression(left, right) {
  override def evaluate(leftResult: ColumnVector, rightResult: ColumnVector): ColumnVector = {
    val v = new BitVector("v", new RootAllocator(Long.MaxValue))
    v.allocateNew()
    0 until leftResult.size foreach { i =>
      val leftValue = leftResult.getValue(i)
      val rightValue = rightResult.getValue(i)
      val value = evaluate(leftValue, rightValue, leftResult.getType)
      v.set(i, if (value) 1 else 0)
    }
    v.setValueCount(leftResult.size)
    ArrowFieldVector(ArrowTypes.BooleanType, v)
  }

  @Override
  def evaluate(l: Any, r: Any, arrowType: ArrowType): Boolean
}

case class EqExpression(
  left: Expression,
  right: Expression
) extends BooleanBinaryExpression(left, right) {
  override def evaluate(l: Any, r: Any, arrowType: ArrowType): Boolean = {
    arrowType match {
      case ArrowTypes.Int8Type => l.asInstanceOf[Byte] == r.asInstanceOf[Byte]
      case ArrowTypes.Int16Type => l.asInstanceOf[Short] == r.asInstanceOf[Short]
      case ArrowTypes.Int32Type => l.asInstanceOf[Int] == r.asInstanceOf[Int]
      case ArrowTypes.Int64Type => l.asInstanceOf[Long] == r.asInstanceOf[Long]
      case ArrowTypes.FloatType => l.asInstanceOf[Float] == r.asInstanceOf[Float]
      case ArrowTypes.DoubleType => l.asInstanceOf[Double] == r.asInstanceOf[Double]
      case ArrowTypes.BooleanType => l.asInstanceOf[Boolean] == r.asInstanceOf[Boolean]
      case ArrowTypes.StringType => BooleanBinaryExpression.toString(l) == BooleanBinaryExpression.toString(r)
    }
  }
}

object BooleanBinaryExpression {
  def toString(v: Any): String = {
    v match {
      case bs: Array[Byte] => new String(bs)
      case null            => null
      case _               => v.toString
    }
  }

  def toBool(v: Any): Boolean = {
    v match {
      case b: Boolean => b
      case n: Number  => n.intValue() == 1
      case _          => throw new IllegalStateException()(s"Cannot convert $v to Boolean")
    }
  }
}

