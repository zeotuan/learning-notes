package datatypes

import org.apache.arrow.vector.types.pojo.ArrowType

/**
 * Represents a column vector in Apache Arrow.
 * This trait defines the basic operations for accessing data in a columnar format.
 */
trait ColumnVector {
  def getType: ArrowType
  def getValue(i: Int): Any
  def size: Int
}
