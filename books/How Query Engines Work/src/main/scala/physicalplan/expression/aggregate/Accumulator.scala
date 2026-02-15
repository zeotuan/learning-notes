package physicalplan.expression.aggregate

trait Accumulator {
  def accumulate(value: Any): Unit
  def finalValue(): Any
}

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

class MinAccumulator extends Accumulator {
  private var currentValue: Option[Any] = None

  override def accumulate(value: Any): Unit = if (value != null) {
    val isMin = currentValue match {
      case None => true
      case Some(cur: Int) => cur > value.asInstanceOf[Int]
      case Some(cur: Long) => cur > value.asInstanceOf[Long]
      case Some(cur: Float) => cur > value.asInstanceOf[Float]
      case Some(cur: Double) => cur > value.asInstanceOf[Double]
      case Some(cur: Short) => cur > value.asInstanceOf[Short]
      case Some(cur: Byte) => cur > value.asInstanceOf[Byte]
      case Some(cur: String) => cur > value.asInstanceOf[String]
      case _ => throw new IllegalArgumentException("Unsupported data type for MaxAccumulator")
    }
    if (isMin) {
      currentValue = Some(value)
    }
  }

  override def finalValue(): Any = currentValue
}

class SumAccumulator extends Accumulator {
  private var currentValue: Option[Any] = None

  override def accumulate(value: Any): Unit = if (value != null) {
    val newSum = currentValue match {
      case None => null
      case Some(cur: Int) => cur + value.asInstanceOf[Int]
      case Some(cur: Long) => cur + value.asInstanceOf[Long]
      case Some(cur: Float) => cur + value.asInstanceOf[Float]
      case Some(cur: Double) => cur + value.asInstanceOf[Double]
      case Some(cur: Short) => cur + value.asInstanceOf[Short]
      case Some(cur: Byte) => cur + value.asInstanceOf[Byte]
      case Some(cur: String) => cur + value.asInstanceOf[String]
      case _ => throw new IllegalArgumentException("Unsupported data type for MaxAccumulator")
    }
    if (newSum != null) {
      currentValue = Some(value)
    }
  }

  override def finalValue(): Any = currentValue
}

class CountAccumulator extends Accumulator {
  private var count: Long = 0L

  override def accumulate(value: Any): Unit = count += 1

  override def finalValue(): Any = count
}

class AvgAccumulator extends Accumulator {
  private val sumAccumulator = new SumAccumulator
  private val countAccumulator = new CountAccumulator

  override def accumulate(value: Any): Unit = {
    value match {
      case Number =>
        sumAccumulator.accumulate(value)
        countAccumulator.accumulate(value)
      case _ => throw new IllegalArgumentException("Unsupported data type for AvgAccumulator")
    }

  }

  override def finalValue(): Any = {
    val sum = sumAccumulator.finalValue()
    val count = countAccumulator.finalValue()
    if (count == 0) {
      null
    } else {
      sum.asInstanceOf[Long] / count.asInstanceOf[Long]
    }

  }
}
