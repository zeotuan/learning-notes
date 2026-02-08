package physicalplan.expression.aggregate

import physicalplan.expression.Expression

trait AggregateExpression {
  def createAccumulator: Accumulator
  def inputExpression: Expression
}
