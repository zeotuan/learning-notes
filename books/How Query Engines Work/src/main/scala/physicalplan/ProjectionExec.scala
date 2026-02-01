package physicalplan

import datasource.DataSource
import datatypes.{RecordBatch, Schema}

class ProjectionExec(input: PhysicalPlan, expression: Seq[expression.Expression], schema: Schema) extends PhysicalPlan {
  override def schema: Schema = schema
  override def children: Seq[PhysicalPlan] = Seq(input)
  override def execute: Iterable[RecordBatch] = input.execute.map { batch =>
    val projectedColumns = expression.map(_.evaluate(batch)).to(IndexedSeq)
    new RecordBatch(schema, projectedColumns)
  }
}
