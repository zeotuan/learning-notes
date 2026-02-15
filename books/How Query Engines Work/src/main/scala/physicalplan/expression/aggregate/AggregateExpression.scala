package physicalplan.expression.aggregate

import physicalplan.expression.Expression

trait AggregateExpression {
  def createAccumulator: Accumulator
  def inputExpression: Expression
}

case class MaxAggregateExpression(inputExpression: Expression) extends AggregateExpression {
  override def createAccumulator: Accumulator = new MaxAccumulator
}

case class MinAggregateExpression(inputExpression: Expression) extends AggregateExpression {
  override def createAccumulator: Accumulator = new MinAccumulator
}

case class SumAggregateExpression(inputExpression: Expression) extends AggregateExpression {
  override def createAccumulator: Accumulator = new SumAccumulator
}

case class CountAggregateExpression(inputExpression: Expression) extends AggregateExpression {
  override def createAccumulator: Accumulator = new CountAccumulator
}

case class AvgAggregateExpression(inputExpression: Expression) extends AggregateExpression {
  override def createAccumulator: Accumulator = new AvgAccumulator
}


