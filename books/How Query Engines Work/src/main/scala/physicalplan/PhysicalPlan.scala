package physicalplan

import datatypes.{RecordBatch, Schema}

trait PhysicalPlan {
  def schema: Schema
  def execute: Iterable[RecordBatch]
  def children: Seq[PhysicalPlan]
}
