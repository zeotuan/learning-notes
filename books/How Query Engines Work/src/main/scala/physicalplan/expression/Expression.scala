package physicalplan.expression

import datatypes.{ColumnVector, RecordBatch}

/*
 * Physical expressions
 */
trait Expression {
  def evaluate(input: RecordBatch): ColumnVector
}
