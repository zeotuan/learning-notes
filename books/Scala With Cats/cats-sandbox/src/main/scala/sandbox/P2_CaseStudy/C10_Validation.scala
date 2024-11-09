package sandbox.P2_CaseStudy

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.instances.list._
import cats.instances.all._
import cats.syntax.validated._
import cats.syntax.apply._

trait Check[E, A] {
  def apply(value: A): Either[E, A]
  def monadAnd(that: Check[E, A]): Check[E, A] = (a: A) => apply(a).flatMap(that(_)) // not good: sequential
  def and(that: Check[E, A]): Check[E, A] = ???  // Use SemiGroupal instead to accumulate errors
}

// We can represent Check as function and Check is just a wrapper around that function

final case class CheckF[E, A](func: A => Either[E, A]) {
  def apply(value: A): Either[E, A] = func(value)

  def and(that: CheckF[E, A])(implicit s: Semigroup[E]): CheckF[E, A] = CheckF { a =>
    (func(a), that(a)) match {
      case (Left(e1), Left(e2)) => Left(e1 |+| e2)
      case (Left(e1), _) => Left(e1)
      case (_, Left(e2)) => Left(e2)
      case _ => Right(a)
    }
  }
}


object CheckF {
  val a: CheckF[List[String], Int] = CheckF { v =>
    if (v > 2) Right(v)
    else Left(List("Must be > 2"))
  }

  val b: CheckF[List[String], Int] = CheckF { v =>
    if (v < -2) Right(v)
    else Left(List("Must be < -2"))
  }

  // Intentionally cannot be both satisfied
  val checkBoth: CheckF[List[String], Int] = a and b

  checkBoth(6) // Left(List("Must be < -2"))
  checkBoth(1) // Left(List("Must be > 2", "Must be < -2"))

  val aNothing: CheckF[Nothing, Int] = CheckF(Right(_))
  val bNothing: CheckF[Nothing, Int] = CheckF(Right(_))
  // val cNothing: CheckF[Nothing, Int] = aNothing and bNothing  -- not possible because there is no Semigroup[Nothing]
}


// Another implementation strategy is to use algebraic data type

/**
 * Using ADT to represent Check is more verbose then function wrapper representation
 * However, it allows us to separate the computation structure (ADT instances we create - definition) from the
 * actual process that give it meaning (the apply method)
 * - ADT instances is similar the Tree of Expression - logical execution plan
 * - the apply method is similar to the interpreter that execute the plan
 * */
sealed trait CheckADT[E, A] {
  import CheckADT._
  def and(that: CheckADT[E, A]): CheckADT[E, A] = And(this, that)
  def or(that: CheckADT[E, A]): CheckADT[E, A] = Or(this, that)
  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => left(a) match {
      case Valid(_) => Valid(a)
      case Invalid(e1) => right(a) match {
        case Valid(_) => Valid(a)
        case Invalid(e2) => Invalid(e1 |+| e2)
      }
    }
  }
}

object CheckADT {
  case class And[E, A](left: CheckADT[E, A], right: CheckADT[E, A]) extends CheckADT[E, A]
  case class Or[E, A](left: CheckADT[E, A], right: CheckADT[E, A]) extends CheckADT[E, A]
  case class Pure[E, A](func: A => Validated[E, A]) extends CheckADT[E, A]
  def pure[E, A](f: A => Validated[E, A]): CheckADT[E, A] = Pure(f)
}

object CheckADTSample {
//  val a: CheckADT[List[String], Int] = CheckADT.pure(v => if (v > 2) Right(v) else Left(List("Must be > 2")))
//  val b: CheckADT[List[String], Int] = CheckADT.pure(v => if (v < -2) Right(v) else Left(List("Must be < -2")))
//  val c:  CheckADT[List[String], Int] = a and b
//
//  c(6) // Left(List("Must be < -2"))
//  c(1) // Left(List("Must be > 2", "Must be < -2"))
}