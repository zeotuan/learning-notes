package sandbox.C6_SemiGroupal_Applicative

import cats.arrow.FunctionK
import cats.{Applicative, Monad, ~>}

/**
 * calling product on type with Monad instance result in sequential semantics
 * It is sometimes not desirable to have this behaviour
 * Parallel allow us to access alternate semantics for certain Monad
 * */
object parallel {
  import cats.Semigroupal
  import cats.instances.either._
  import cats.syntax.parallel._

  type ErrorOr[A] = Either[Seq[String], A]
  val error1: ErrorOr[Int] = Left(Seq("Error 1"))
  val error2: ErrorOr[Int] = Left(Seq("Error 2"))
  (error1, error2).parTupled // Left(Seq("Error 1", "Error 2"))

  /**
   * this allows us to convert a type with a Monad instance
   * to a related type with an Applicative (Semigroupal) instance that has alternate semantics for product
   * */
  trait RawParallel[M[_]] {
    type F[_]
    def applicative: Applicative[F] // There must be a related type constructor F that has an Applicative instance
    def monad: Monad[M] // There must be a Monad instance for M
    /**
     * We can convert M to F
     * ~> is type alias for FunctionK
     * A => B is a function that convert from value of type A to value of type B
     * M and F are type constructor not type
     * a FunctionK M ~> F is a function from M[A] to F[A]
     * */
    def parallel: ~>[M, F]
  }

  // Example of FunctionK from  Option to List
  // more manual way
  val optionToList = new FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] = fa match {
      case Some(a) => List(a)
      case None => List.empty[A]
    }
  }
  // newer syntax
  val optionToList1 = new (Option ~> List) {
    def apply[A](fa: Option[A]): List[A] = fa.toList
  }
}

