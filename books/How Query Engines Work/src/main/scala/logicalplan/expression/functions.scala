package logicalplan.expression

object functions {
  def col(name: String): Column = {
    Column(name)
  }

  def sum(input: LogicalExpr): Sum = {
    Sum(input)
  }

  def avg(input: LogicalExpr): Avg = {
    Avg(input)
  }

  def min(input: LogicalExpr): Min = {
    Min(input)
  }

  def max(input: LogicalExpr): Max = {
    Max(input)
  }

  def count(input: LogicalExpr): Count = {
    Count(input)
  }
}
