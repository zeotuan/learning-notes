package physicalplan

import datatypes.{RecordBatch, Schema}

case class ProjectionExec(input: PhysicalPlan, expression: Seq[expression.Expression], cusSchema: Schema) extends PhysicalPlan {
  override def schema: Schema = cusSchema
  override def children: Seq[PhysicalPlan] = Seq(input)
  override def execute: Iterable[RecordBatch] = input.execute.map { batch =>
    val projectedColumns = expression.map(_.evaluate(batch)).to(IndexedSeq)
    new RecordBatch(schema, projectedColumns)
  }
}
