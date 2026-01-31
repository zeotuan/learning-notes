package physicalplan.expression.aggregate

trait Accumulator {
  def accumulate(value: Any): Unit
  def finalValue(): Any
}
