package physicalplan.expression

import datatypes.{ColumnVector, RecordBatch}

abstract class BinaryExpression(left: Expression, right: Expression) extends Expression {
  final def evaluate(input: RecordBatch): ColumnVector = {
    val leftResult = left.evaluate(input)
    val rightResult = right.evaluate(input)
    assert(
      leftResult.size == rightResult.size,
      s"Left and right expressions must have the same size: ${leftResult.size} != ${rightResult.size}"
    )

    if (leftResult.getType != rightResult.getType) {
      throw new IllegalStateException("Left and right expressions must have the same data type")
    }

    evaluate(leftResult, rightResult)
  }

  /**
   * Evaluate left and right ColumnVector.
   */
  @Override
  def evaluate(leftResult: ColumnVector, rightResult: ColumnVector): ColumnVector
}
