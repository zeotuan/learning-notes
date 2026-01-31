package physicalplan.expression.aggregate

class MaxAccumulator extends Accumulator {
  private var currentValue: Option[Any] = None

  override def accumulate(value: Any): Unit = if (value != null) {
    val isMax = currentValue match {
      case None => true
      case Some(cur: Int) => cur < value.asInstanceOf[Int]
      case Some(cur: Long) => cur < value.asInstanceOf[Long]
      case Some(cur: Float) => cur < value.asInstanceOf[Float]
      case Some(cur: Double) => cur < value.asInstanceOf[Double]
      case Some(cur: Short) => cur < value.asInstanceOf[Short]
      case Some(cur: Byte) => cur < value.asInstanceOf[Byte]
      case Some(cur: String) => cur < value.asInstanceOf[String]
      case _ => throw new IllegalArgumentException("Unsupported data type for MaxAccumulator")
    }
    if (isMax) {
      currentValue = Some(value)
    }
  }

  override def finalValue(): Any = currentValue
}
