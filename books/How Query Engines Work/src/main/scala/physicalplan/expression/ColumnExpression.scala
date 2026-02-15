package physicalplan.expression

import datatypes.{ColumnVector, RecordBatch}

/*
 * Physical expressions
 */
case class ColumnExpression(columnIndex: Int) extends Expression {
  def evaluate(input: RecordBatch): ColumnVector = input.field(columnIndex)
}
