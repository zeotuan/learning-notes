package datatypes

import org.apache.arrow.vector.types.FloatingPointPrecision
import org.apache.arrow.vector.types.pojo.ArrowType

object ArrowTypes {
  val BooleanType: ArrowType = ArrowType.Bool
  val Int8Type: ArrowType = new ArrowType.Int(8, true)
  val Int16Type: ArrowType = new ArrowType.Int(16, true)
  val Int32Type: ArrowType = new ArrowType.Int(32, true)
  val Int64Type: ArrowType = new ArrowType.Int(64, true)
  val UInt8Type: ArrowType = new ArrowType.Int(8, false)
  val UInt16Type: ArrowType = new ArrowType.Int(16, false)
  val UInt32Type: ArrowType = new ArrowType.Int(32, false)
  val UInt64Type: ArrowType = new ArrowType.Int(64, false)
  val FloatType: ArrowType = new ArrowType.FloatingPoint(FloatingPointPrecision.SINGLE)
  val DoubleType: ArrowType = new ArrowType.FloatingPoint(FloatingPointPrecision.DOUBLE)
  val StringType: ArrowType = new ArrowType.Utf8()
}
