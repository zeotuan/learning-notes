package datatypes

import org.apache.arrow.memory.RootAllocator
import org.apache.arrow.vector._
import org.apache.arrow.vector.types.pojo.ArrowType

object FieldVector {
  val name = "v"
  def apply(arrowType: ArrowType, initialCapacity: Int): FieldVector = {
    val rootAllocator = new RootAllocator(Long.MaxValue)
    val fieldVector = arrowType match {
      case ArrowTypes.BooleanType => new BitVector(name, rootAllocator)
      case ArrowTypes.Int8Type => new TinyIntVector(name, rootAllocator)
      case ArrowTypes.Int16Type => new SmallIntVector(name, rootAllocator)
      case ArrowTypes.Int32Type => new IntVector(name, rootAllocator)
      case ArrowTypes.Int64Type => new BigIntVector(name, rootAllocator)
      case ArrowTypes.FloatType => new Float4Vector(name, rootAllocator)
      case ArrowTypes.DoubleType => new Float8Vector(name, rootAllocator)
      case ArrowTypes.StringType => new VarCharVector(name, rootAllocator)
      case _ => throw new UnsupportedOperationException(s"Unsupported arrow type: $arrowType")
    }

    if (initialCapacity != 0) {
      fieldVector.setInitialCapacity(initialCapacity)
    }
    fieldVector.allocateNew()
    fieldVector
  }

  implicit class FieldVectorOps(val vector: FieldVector) extends AnyVal {
    def getArrowType: ArrowType = vector match {
      case _: BitVector => ArrowTypes.BooleanType
      case _: TinyIntVector => ArrowTypes.Int8Type
      case _: SmallIntVector => ArrowTypes.Int16Type
      case _: IntVector => ArrowTypes.Int32Type
      case _: BigIntVector => ArrowTypes.Int64Type
      case _: Float4Vector => ArrowTypes.FloatType
      case _: Float8Vector => ArrowTypes.DoubleType
      case _: VarCharVector => ArrowTypes.StringType
      case _                => throw new IllegalStateException(s"Unsupported vector type: ${vector.getClass.getSimpleName}")
    }

    def setValue(i: Int, value: Any): Unit = {
      vector match {
        case v: BitVector      => setBit(v, i, value)
        case v: TinyIntVector  => setTinyInt(v, i, value)
        case v: SmallIntVector => setSmallInt(v, i, value)
        case v: IntVector      => setInt(v, i, value)
        case v: BigIntVector   => setBigInt(v, i, value)
        case v: Float4Vector   => setFloat(v, i, value)
        case v: Float8Vector   => setDouble(v, i, value)
        case v: VarCharVector  => setVarChar(v, i, value)
        case _                 => throw new IllegalStateException(s"Unsupported vector type: ${vector.getClass.getSimpleName}")
      }
    }
  }

  private def setVarChar(vector: VarCharVector, i: Int, value: Any): Unit = value match {
    case null => vector.setNull(i)
    case bytes: Array[Byte] => vector.set(i, bytes)
    case v => vector.set(i, v.toString.getBytes)
  }

  private def setTinyInt(vector: TinyIntVector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Byte    => vector.set(i, v)
    case v: Number  => vector.set(i, v.byteValue())
    case v: String  => vector.set(i, v.toByte)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Byte")
  }

  private def setSmallInt(vector: SmallIntVector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Short   => vector.set(i, v)
    case v: Number  => vector.set(i, v.shortValue())
    case v: String  => vector.set(i, v.toShort)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Short")
  }

  private def setInt(vector: IntVector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Int     => vector.set(i, v)
    case v: Number  => vector.set(i, v.intValue())
    case v: String  => vector.set(i, v.toInt)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Int")
  }

  private def setBigInt(vector: BigIntVector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Long    => vector.set(i, v)
    case v: Number  => vector.set(i, v.longValue())
    case v: String  => vector.set(i, v.toLong)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Long")
  }

  private def setFloat(vector: Float4Vector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Float   => vector.set(i, v)
    case v: Number  => vector.set(i, v.floatValue())
    case v: String  => vector.set(i, v.toFloat)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Float")
  }

  private def setDouble(vector: Float8Vector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Double  => vector.set(i, v)
    case v: Number  => vector.set(i, v.doubleValue())
    case v: String  => vector.set(i, v.toDouble)
    case _ => throw new IllegalStateException(s"Cannot convert $value to Double")
  }

  private def setBit(vector: BitVector, i: Int, value: Any): Unit = value match {
    case null       => vector.setNull(i)
    case v: Boolean => vector.set(i, if (v) 1 else 0)
    case v: Number  => vector.set(i, if (v.intValue() > 0) 1 else 0)
    case _          => throw new IllegalStateException(s"Cannot convert $value to Boolean")
  }
}
