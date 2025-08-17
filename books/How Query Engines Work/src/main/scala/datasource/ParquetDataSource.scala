package datasource

import datatypes.{RecordBatch, Schema}

case class ParquetDataSource() extends DataSource {
  def schema: Schema = ???
  def scan(projection: Seq[String]): Iterator[RecordBatch] = ???
}
