package datatypes

import org.apache.arrow.vector._
import org.apache.arrow.vector.types.pojo.ArrowType

case class ArrowFieldVector(
  arrowType: ArrowType,
  value: FieldVector,
) extends ColumnVector {
  def getType: ArrowType = arrowType

  def getValue(i: Int): Any = {
    value match {
      case vector: BitVector
        => if (vector.get(i) == 1) true else false

      case vector: TinyIntVector | SmallIntVector | IntVector | BigIntVector |
                   Float2Vector | Float4Vector | Float8Vector
        => vector.get(i)

      case vector: VarCharVector =>
        val bytes = vector.get(i)
        if (bytes == null) null else bytes.toString

      case _ =>
        throw new UnsupportedOperationException(s"Unsupported vector type: ${value.getClass.getSimpleName}")
    }
  }

  def size: Int = value.getValueCount
}