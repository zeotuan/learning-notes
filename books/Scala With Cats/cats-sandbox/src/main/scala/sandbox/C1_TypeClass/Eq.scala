package sandbox.C1_TypeClass

import cats.Eq
import cats.implicits._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.option._

import java.time.LocalDate

//trait Eq[A] {
//  def eqv(a: A, b: A): Boolean
//}

object EqExample {

  eqInt.eqv(123, 123)
  eqInt.eqv(123, 124)


  val eqInt = Eq[Int]

  List(1,2,3).map(Option(_)).filter(i => i === none[Int])
  List(1,2,3).map(Option(_)).filter(i => i === 1.some)
  List(1,2,3).map(Option(_)).filter(i => i =!= 2.some)

  implicit val dateEq: Eq[LocalDate] = Eq.instance[LocalDate] { (date1, date2) => date1.toEpochDay === date2.toEpochDay }

  LocalDate.now() === LocalDate.now()

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) => cat1 equals cat2 }
}