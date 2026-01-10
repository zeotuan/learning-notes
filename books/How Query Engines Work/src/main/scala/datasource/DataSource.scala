package datasource

import datatypes.{RecordBatch, Schema}

trait DataSource {
  def schema: Schema
  def scan(projection: Seq[String]): Iterable[RecordBatch]
}
