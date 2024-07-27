package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter13IO.Monad

import scala.util.{Failure, Success, Try}

/**
 * [[MonadThrow]] extends [[Monad]] with the ability to raise error and later handle them
 * */
trait MonadThrow[F[_]] extends Monad[F] {
  implicit class MonadThrowOps[A](fa: F[A]) {
    def attempt: F[Try[A]] = ???
    def handleErrorWith(f: Throwable => F[A]): F[A] = attempt flatMap {
      case Failure(e) => f(e)
      case Success(a) => unit(a)
    }
  }
  def raiseError[A](t: Throwable): F[A] = ???
}
