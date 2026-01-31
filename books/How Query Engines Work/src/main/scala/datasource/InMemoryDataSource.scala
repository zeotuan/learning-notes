package datasource

import datatypes.{RecordBatch, Schema}

case class InMemoryDataSource(
  customSchema: Schema,
  data: Seq[RecordBatch]
) extends DataSource {
  def schema: Schema = customSchema
  def scan(projection: Seq[String]): Iterable[RecordBatch] = {
    val dataIterator = if (projection.isEmpty) {
      data.iterator
    } else {
      val readSchema = customSchema.select(projection)
      data.iterator.map { batch =>
        val projectedFields = projection.map { name =>
          val index = customSchema.fields.indexWhere(_.name == name)
          batch.field(index)
        }
        new RecordBatch(readSchema, projectedFields.toIndexedSeq)
      }
    }
    dataIterator.to(Iterable)
  }
}

object InMemoryDataSource {
  def apply(): InMemoryDataSource = {
    InMemoryDataSource(
      Schema(Seq.empty),
      Seq.empty
    )
  }
}
