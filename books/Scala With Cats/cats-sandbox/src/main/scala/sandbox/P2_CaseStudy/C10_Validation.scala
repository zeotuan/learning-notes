package sandbox.P2_CaseStudy

import cats.Semigroup
import cats.data.{NonEmptyList, Validated}
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.either._
import cats.syntax.semigroup._
import cats.instances.list._
import cats.instances.all._
import cats.syntax.validated._
import cats.syntax.apply._
import sandbox.P2_CaseStudy.CheckADTSample.{alphanumeric, contains, containsOnce, longerThan}

trait Check1[E, A] {
  def apply(value: A): Either[E, A]
  def monadAnd(that: Check1[E, A]): Check1[E, A] = (a: A) => apply(a).flatMap(that(_)) // not good: sequential
  def and(that: Check1[E, A]): Check1[E, A] = ???  // Use SemiGroupal instead to accumulate errors
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
sealed trait Predicate[E, A] {
  import Predicate._
  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
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
  def run(a: A)(implicit s: Semigroup[E]): Either[E, A] = apply(a).toEither
}

object Predicate {
  final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]

  def lift[E, A](err: E, fn: A => Boolean): Predicate[E, A] =
    Pure(a => if (fn(a)) a.valid else err.invalid)

  def apply[E, A](func: A => Validated[E, A]): Predicate[E, A] = Pure(func)
}

sealed trait Check[E, A, B] {
  import Check._

  def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]
  def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)
  def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)
  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)
}

object Check {
  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).map(f)
  }
  final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check(in).withEither(_.flatMap(b => f(b)(in).toEither))
  }
  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] = check1(in).withEither(_.flatMap(b => check2(b).toEither))
  }
  final case class Pure[E, A, B](func: A => Validated[E, B]) extends Check[E, A, B] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B] = func(in)
  }
  final case class PurePredicate[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] = pred(in)
  }

  def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = PurePredicate(pred)
  def apply[E, A, B](func: A => Validated[E, B]): Check[E, A, B] = Pure(func)
}

object CheckADTSample {
  import cats.data.{NonEmptyList, Validated}
  type Errors = NonEmptyList[String]
  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
    error(s"Must be longer than $n characters"),
    str => str.size > n
  )
  val alphanumeric: Predicate[Errors, String] = Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit)
  )
  def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char)
  )
  def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.filter(c => c == char).size == 1
  )

  val checkUsername: Check[Errors, String, String] = Check(longerThan(3) and alphanumeric)
  val checkPassword: Check[Errors, String, String] = Check(longerThan(8) and containsOnce('!') and containsOnce('?'))
  val splitEMail: Check[NonEmptyList[String], String, (String, String)] = Check { email =>
     email.split('@') match {
      case Array(name, domain) => (name, domain).valid
      case _ => error("Must contain a single @ character").invalid
    }
  }

  val checkEmailName: Check[Errors, String, String] = Check(longerThan(6))
  val checkEmailDomain: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))
  val joinEmail: Check[Errors, (String, String), String] = Check[Errors, (String, String), String] {
    case (l, r) =>  (checkEmailName(l), checkEmailDomain(r)).mapN((l, r) => l + "@" +  r)
  }

  val checkEMail: Check[Errors, String, String] = splitEMail andThen joinEmail

  final case class User(username: String, password: String, email: String)
  def createUser(username: String, password: String, email: String): Validated[Errors, User] = {
    (checkUsername(username), checkPassword(password), checkEMail(email)).mapN(User)
  }
}

/**
 * The Predicate we had written is essentially a function A => Validated[E, A]
 * Check is a wrapper around Predicate that lets us compose them
 * A more abstract concept that captures this idea is Kleisli arrow
 * Kleisli is a wrapper around function A => F[B]
 * This allows us to have this sequencing monadic transformation:
 * A => F[A] flatMap A => F[B] flatMap B => F[C] = A => F[C]
 *
 * */
object KleisliSample {
  import cats.data.Kleisli
  import cats.instances.list._

  val step1: Kleisli[List, Int, Int] = Kleisli(x => List(x + 1, x - 1))
  val step2: Kleisli[List, Int, Int] = Kleisli(x => List(x, -x))
  val step3: Kleisli[List, Int, Int] = Kleisli(x => List(x * 2, x / 2))
  val pipelines = step1 andThen step2 andThen step3
  pipelines.run(20) // List(42, 10, 21, -10, 40, 10, 19, -9)


  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type CheckK[A, B] = Kleisli[Result, A, B]

  // we no longer need to define the Check ADT type to compose the predicates
  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)
  def check[A, B](func: A => Result[B]): CheckK[A, B] = Kleisli(func)
  def checkPred[A](pred: Predicate[Errors, A]): CheckK[A, A] = Kleisli[Result, A, A](pred.run)

  val checkUsername: CheckK[String, String] = checkPred(longerThan(3) and alphanumeric)
  val checkPassword: CheckK[String, String] = checkPred(longerThan(8) and containsOnce('!') and containsOnce('?'))
  val splitEMail: CheckK[String, (String, String)] = check { email =>
    email.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => error("Must contain a single @ character").asLeft
    }
  }

  val checkEmailName: CheckK[String, String] = checkPred(longerThan(6))
  val checkEmailDomain: CheckK[String, String] =checkPred(longerThan(3) and contains('.'))
  val joinEmail: CheckK[(String, String), String] = check {
    case (l, r) =>  (checkEmailName(l), checkEmailDomain(r)).mapN((l, r) => l + "@" +  r)
  }

  val checkEMail: CheckK[String, String] = splitEMail andThen joinEmail

  final case class User(username: String, password: String, email: String)
  def createUser(username: String, password: String, email: String): Result[User] = {
    (checkUsername(username), checkPassword(password), checkEMail(email)).mapN(User)
  }
}
