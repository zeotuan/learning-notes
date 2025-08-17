package physicalplan

import datatypes.{RecordBatch, Schema}

trait PhysicalPlan {
  def schema: Schema
  def execute(): Iterator[RecordBatch]
  def children: Seq[PhysicalPlan]
}
