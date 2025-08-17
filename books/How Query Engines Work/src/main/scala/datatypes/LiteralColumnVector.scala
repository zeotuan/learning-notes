package datatypes

import org.apache.arrow.vector.types.pojo.ArrowType

case class LiteralColumnVector[T](
  arrowType: ArrowType,
  value: T,
  valueSize: Int = 0
) extends ColumnVector {
  def getType: ArrowType = arrowType

  def getValue(i: Int): T = {
    if (i<0 || i>=size) {
      throw new IndexOutOfBoundsException()
    }
    value
  }

  def size: Int = valueSize
}
