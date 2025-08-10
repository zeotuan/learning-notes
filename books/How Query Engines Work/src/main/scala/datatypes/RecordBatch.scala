package datatypes

/* *
 * RecordBatch represents a batch of columnar data
 */
class RecordBatch(val schema: Schema, val fields: IndexedSeq[ColumnVector]) {
  def rowCount: Int = fields.head.size
  def columnCount: Int = fields.size
  def field(i: Int): ColumnVector = fields(i)
}
