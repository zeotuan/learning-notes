package physicalplan

import datasource.DataSource
import datatypes.{RecordBatch, Schema}

case class ScanExec(ds: DataSource, projection: Seq[String]) extends PhysicalPlan {
  override def schema: Schema = ds.schema.select(projection)
  override def children: Seq[PhysicalPlan] = Seq.empty
  override def execute: Iterable[RecordBatch] = ds.scan(projection)
}
