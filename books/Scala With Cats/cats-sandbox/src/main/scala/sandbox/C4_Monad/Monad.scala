package sandbox.C4_Monad

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Try

object Raw {
  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  }
}

object CatMonad {
  import cats.implicits._
  import cats.instances.list._ // for Functor
  import cats.instances.function._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.Monad

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => a * 100)


  import scala.concurrent.ExecutionContext.Implicits.global
  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(a => fm.pure(a + 2))
  Await.result(future, 1.second)


  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(a1 => b.map(b1 => a1 * a1 + b1 * b1))
  sumSquare(Option(1), Option(2))
  sumSquare(List(1,2,3), List(4,5,6))

  def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = for {
    a1 <- a
    b1 <- b
  } yield a1 * a1 + b1 * b1

  import cats.Id

  sumSquare2(3: Id[Int], 4: Id[Int])
  val a = Monad[Id].pure(3)
  // a: Id[Int] = 3
  val b = Monad[Id].flatMap(a)(_ + 1)
  // b: Id[Int] = 4
  val c = for {
    x <- a
    y <- b
  } yield x + y

  // Identity Monad
  def idMonad: Raw.Monad[Id] = new Raw.Monad[Id] {
    override def pure[A](x: A): Id[A] = Id(x)
    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = ???
  }
}

object CatEitherMonad {
  val ei1: Either[String, Int] = Right(10)
  val ei2: Either[String, Int] = Right(32)
  val ei3 = for {
    a <- ei1
    b <- ei2
  } yield a + b


  import cats.syntax.either._ // for asRight "smart constructor" allow us to return an Either and help with type inference compare to Right/Left.apply
  val ei4 = 3.asRight[String]
  val ei5 = 4.asRight[String]
  val ei6 = for {
    a <- ei4
    b <- ei5
  } yield a * a + b * b


  def countPositive(nums: List[Int]) = nums.foldLeft(0.asRight[String]) { (acc, num) =>
    if (num > 0)
      acc.map(_ + 1)
    else
      Left("Negative. Stopping")
  }

  // some extension methods provide by cats
  Either.catchOnly[NumberFormatException]("foo".toInt)
  Either.catchNonFatal(sys.error("System err"))
  Either.fromTry(Try()) == Try().toEither
  Either.fromOption(None, "Badness")

  // built in getOrElse and orElse
  "Error".asLeft[Int].getOrElse(0)
  "Error".asLeft[Int].orElse(2.asRight[String])

  val ei8 = (-1).asRight[String] match {
    case Left(value) => Left(value)
    case Right(_) => Left("Must be non-negative")
    case Right(value) if value > 0 =>  Right(value)
  }

  // Cats provide a ensure helper method to simplify above code
  val ei7 = (-1).asRight[String].ensure("Must be non-negative")(_ > 0)


  // Cats also provide a recover method to recover from error for certain cases
  // Here we recover all cases to -1
  "error".asLeft[Int].recover { case _: String => -1 }

  // similar to recover buth allow return a Right instead of the wrapped value
  "error".asLeft[Int].recoverWith { case _: String => Right(-1) }

  // Map on left value Left("oof")
  "foo".asLeft[Int].leftMap(_.reverse)

  // bimap on both right and lep
   6.asRight[String].bimap(_.reverse, _ * 7) // Right(42) but also map on both value
  "foo".asLeft[Int].bimap(_.reverse, _ * 7) // Left("oof")

  // swap left and right type
  6.asRight[String].swap // Left(6) - Either[Int, String]


  // Using either for fail fast
  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "Div by zero".asLeft[Int] else (a / b).asRight[String]
    d <- "foo".asLeft[Int]
    e = c * d
    f <- 2.asRight[String]
  } yield  e / f

  import ErrorWrapper._
  case  class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  User("dave",  "passw0rd").asRight.fold(handleError, println)
  UserNotFound("dave").asLeft.fold(handleError, println)

}

// an approach to represent error other than Throwable which can be too broad
object ErrorWrapper {
  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class passwordIncorrect(username: String) extends LoginError
  case object UnexpectedLoginError extends LoginError

  def handleError(error: LoginError): Unit = error match {
    case UserNotFound(u) => println(s"User not found: $u")
    case passwordIncorrect(u) => println(s"Password incorrect: $u")
    case UnexpectedLoginError => println(s"Unexpected error")
  }
}