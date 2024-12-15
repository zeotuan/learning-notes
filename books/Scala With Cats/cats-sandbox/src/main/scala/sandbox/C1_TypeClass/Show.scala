package sandbox.C1_TypeClass


import java.time.LocalDate

/**
 * trait Show[A] {
 *  def show(value: A): String
 * }
 * */

object ShowExample {
  import cats.Show
  import cats.implicits._
  val showInts = Show.apply[Int]

  // implicit val showDate: Show[LocalDate] = (t: LocalDate) => s"${t.toEpochDay} days since epoch"
  // "shorter" alternative to the above
  implicit val showDate2: Show[LocalDate] = Show.show(date => s"${date.toEpochDay} days since epoch")

  LocalDate.MAX.show
}