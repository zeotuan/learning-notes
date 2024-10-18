package sandbox.C5_Transformative

import cats.data.EitherT

import scala.concurrent.Future

object MonadTransformer {
  import cats.data.OptionT
  import cats.instances.list._
  import cats.syntax.applicative._
  type ListOption[A] = OptionT[List, A] // essentially List[Option[A]]
  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]
  val result3: ListOption[Int] = result1.flatMap(x => result2.map(y => x + y))
  val result4: ListOption[Int] = for {
    x <- result1
    y <- result2
  } yield x + y

  // Monad Transformer let us combine map and flatmap methods without having to
  // unwrap and rewrap the value at each state of the transformation
  // each monad transformer is a data type that allows us to wrap stacks of monads
  // together to produce a new monad
  // many monad in cats are defined by combining a transformer with a Id monad
  // Reader monad is a specialization of a more general concept called kleisli arrow

  type ErrorOr[A] = Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A] // Either[String, Option[A]]
  val a = 10.pure[ErrorOrOption] // OptionT(Right(Some(10)))
  val b = 32.pure[ErrorOrOption] // OptionT(Right(Some(32)))
  val c = for {
    x <- a
    y <- b
  } yield x + y // OptionT(Right(Some(42)))

  type FutureEither[A] = EitherT[Future, String, A] // Future[Either[String, A]]
  type FutureEitherOption[A] = OptionT[FutureEither, A] // Future[Either[String, Option[A]]]

  import cats.instances.future._
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr = for {
    a <- 10.pure[FutureEitherOption]
    b <- 32.pure[FutureEitherOption]
  } yield a + b

  // We can use kind projector plugin to make the syntax more concise by reducing the number of
  // intermediate types we need to specify
  import cats.instances.option._
  123.pure[EitherT[Option, String, *]] // EitherT[Option, String, Int]

  // we can use value to extract the final value from the monad transformer
  // each call to value will unwrap the outer monad in the transformer stack
  // the larger the stack, the more calls to value we need to extract the value
  val aVal = a.value // ErrorOr[Option[Int]] = Right(Some(10))
  val bVal = b.value // ErrorOr[Option[Int]] = Right(Some(32))
  val futureEitherOrVal: FutureEither[Option[Int]] = futureEitherOr.value
  val futureEitherOrValVal: Future[Either[String, Option[Int]]]  = futureEitherOrVal.value
  Await.result(futureEitherOrValVal, 2.seconds) // Right(Some(42))
}

object MonadTransformerGlueCodeExample {
  import cats.data.Writer
  import cats.data.OptionT
  import scala.util.{Try, Failure, Success}
  type Logged[A] = Writer[List[String], A]

  // general raw method that return untransformed stack
  def parseNumber(str: String): Logged[Option[Int]] = Try(str.toInt) match {
    case Success(value) => Writer(List(s"parsed $str"), Some(value))
    case Failure(exception) => Writer(List(s"Failed to parse $str"), None)
  }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    // local usage of monadTransformer to simplify composition
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }

  // normal code are not forced to use OptionT
  val result1 = addAll("1", "2", "3")
  val result2 = addAll("1", "a", "3")
}
