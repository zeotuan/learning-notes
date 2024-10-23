package sandbox.C6_SemiGroupal_Applicative

/**
 * flatMap and map are useful but there are some flows that they cannot express
 * For example:
 * - form validation where we want to accumulate all errors instead of early termination
 * - parallel processing where we want to run multiple tasks and combine the results since
 *   flatMap is sequential where each step depends on the previous one
 *
 * Some other type classes that can help us with these limitations are:
 * - SemiGroupal: Encompasses the idea of combining independent contexts
 * - Parallel: convert a type with a Monad to a related type with a SemiGroupal instance
 * - Applicative extends Semigroupal and Functor and the source of pure method
 * */
object flatMapLimitations {
  import cats.syntax.either._
  def parseInt(str: String): Either[String, Int] = {
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")
  }

  for {
    a <- parseInt("1")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield a + b + c // Either[String, Int] = Left("Couldn't read b")
}

/**
 * A type that allow combining of independent contexts
 * */
trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

object SemiGroupal {
  import cats.Semigroupal
  import cats.instances.option._

  Semigroupal[Option].product(Some(123), Some("abc")) // Some((123, "abc"))
  Semigroupal[Option].product(None, Some("abc")) // None
  Semigroupal[Option].product(Some(123), None) // None
}
