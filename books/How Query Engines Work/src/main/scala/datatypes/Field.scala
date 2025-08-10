package datatypes

import org.apache.arrow.vector.types.pojo.{ArrowType, FieldType}

case class Field(name: String, dataType: ArrowType) {
  def toArrow: org.apache.arrow.vector.types.pojo.Field = {
    val fieldType = new FieldType(true, dataType, null)
    new org.apache.arrow.vector.types.pojo.Field(name, fieldType, null)
  }
}
