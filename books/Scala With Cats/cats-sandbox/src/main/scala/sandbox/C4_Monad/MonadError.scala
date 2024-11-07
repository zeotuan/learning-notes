package sandbox.C4_Monad

object Raw {
  import sandbox.C4_Monad.Raw
  /**
   * @tparam F Monad type
   * @tparam E Error type
   * */
  trait MonadError[F[_], E] extends Raw.Monad[F] {
    // lift an error into the `F` context
    def raiseError[A](e: E): F[A]

    // handle an error, potentially recovering from it
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]

    def handleError[A](fa: F[A])(f: E => A): F[A] =
      handleErrorWith(fa)(f andThen pure)

    def ensure[A](fa: F[A])(e: E)(f: A => Boolean): F[A]
  }
}
object CatMonadError {

  import cats.MonadError
  import cats.instances.either._ // for MonadError

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42) // Right(42)
  val failure = monadError.raiseError("Badness") // Left("Badness")

  // Handle some error
  val handledFailure: ErrorOr[String] = monadError.handleErrorWith(failure) {
    case "Badness" => monadError.pure("It's ok")
    case _ => monadError.raiseError("It's not ok")
  }

  // handle all error
  val handledFailure1: ErrorOr[Int] = monadError.handleError[Int](failure) {
    case "Badness" => 42
    case _ => -1
  }

  monadError.ensure(success)("Number too low!")(_ > 100)

  // we can use cats syntax to make it more concise
  //
  import cats.syntax.applicative._ // Applicative pure
  import cats.syntax.applicativeError._ // ApplicativeError raiseError
  import cats.syntax.monadError._ // MonadError ensure

  val success1 = 42.pure[ErrorOr]

  val failure1 = "Badness".raiseError[ErrorOr, Int]
  val catHandledFailure1 = failure1.handleErrorWith {
    case "Badness" => 42.pure
    case _ => "It's not ok".raiseError
  }

  val catHandledFailure2 = failure1.handleError {
    case "Badness" => 42
    case _ => -1
  }

  import scala.util.Try
  import cats.instances.try_._

  // val t: Try[Int] = Try(throw new RuntimeException("Badness")) - Similar but you don't have to explicitly tell compiler the type
  new RuntimeException("Badness").raiseError[Try, Int] // Try[Int] = Failure(java.lang.RuntimeException: Badness)

  def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] = {
    age.pure[F].ensure(new RuntimeException("Age must be greater than or equal to 18"))(_ >= 18)
  }

  validateAdult[Try](18) // Success(18)
}
