package datasource

import datatypes.{RecordBatch, Schema}

case class InMemoryDataSource() extends DataSource {
  def schema: Schema = ???
  def scan(projection: Seq[String]): Iterator[RecordBatch] = ???
}
