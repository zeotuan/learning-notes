package exercieAnswers.chapter04ExceptionHandling

import scala.{Either => _, Option => _}

sealed trait Validated[+E, +A] {
  def toEither: Either[List[E], A] = this match {
    case Invalid(es) => Left(es)
    case Valid(a) => Right(a)
  }

  def map[B](f: A => B): Validated[E, B] = this match {
    case Valid(a) => Valid(f(a))
    case Invalid(es)  => Invalid(es)
  }

  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C): Validated[EE, C] = (this, b) match {
    case (Valid(a), Valid(b)) => Valid(f(a, b))
    case (Invalid(e), Valid(_)) => Invalid(e)
    case (Valid(_), Invalid(e)) => Invalid(e)
    case (Invalid(e), Invalid(e1)) => Invalid(e ++ e1)
  }
}

case class Valid[A](get: A) extends Validated[Nothing, A]
case class Invalid[E](get: List[E]) extends Validated[E, Nothing]

object Validated {
  def fromEither[E, A](e: Either[List[E], A]):Validated[E, A] = e match {
    case Left(es) => Invalid(es)
    case Right(a) => Valid(a)
  }

  def traverse[E, A, B](as: List[A])(f: A => Validated[E, B]): Validated[E, List[B]] = as.foldRight(Valid(Nil): Validated[E, List[B]])((a, b) => f(a).map2(b)(_ :: _))
  def sequence[E,A](as: List[Validated[E, A]]): Validated[E, List[A]] = traverse(as)(a => a)
}