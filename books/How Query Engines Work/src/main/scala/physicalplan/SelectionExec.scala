package physicalplan

import datatypes.FieldVector.FieldVectorOps
import datatypes.{ArrowFieldVector, ColumnVector, FieldVector, RecordBatch, Schema}
import org.apache.arrow.vector.BitVector

class SelectionExec(input: PhysicalPlan, expression: expression.Expression) extends PhysicalPlan {
  override def schema: Schema = schema
  override def children: Seq[PhysicalPlan] = Seq(input)
  override def execute: Iterable[RecordBatch] = input.execute.map { batch =>
    val schema = batch.schema
    val columnsCount = schema.fields.length
   expression.evaluate(batch) match {
    case ArrowFieldVector(bitVec: BitVector) =>
      val filteredFields = (0 until columnsCount).map { i => filter(batch.field(i), bitVec) }
      new RecordBatch(schema, filteredFields)
    case other => throw new UnsupportedOperationException(s"Unsupported filter vector type: $other")
    }
  }

  private def filter(v: ColumnVector, selectionVector: BitVector): ColumnVector = {
    val filteredValue = (0 until selectionVector.getValueCount).foldLeft(Seq.empty[Any]) {
      case (curr, i ) =>
        if (selectionVector.get(i) == 1) {
          curr :+ v.getValue(i)
        } else {
          curr
        }
    }
    val filterVector = FieldVector(v.getType, filteredValue.length)
    filterVector.allocateNew()

    filteredValue.zipWithIndex.foreach { case (v, i) => filterVector.setValue(i, v) }
    filterVector.setValueCount(filteredValue.length)
    ArrowFieldVector(filterVector)
  }
}
