package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter13IO.IOApp.IO
import exercieAnswers.chapter13IO.{Monad, Task}

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

object MonadThrow {
  implicit val taskInstance: MonadThrow[Task] = new MonadThrow[Task] {
    override def unit[A](a: => A): Task[A] = Task( IO { Try(a) })

    override def flatMap[A, B](a: Task[A])(f: A => Task[B]): Task[B] = ???

    implicit class TaskMonadThrowOps[A](fa: Task[A]) {
      def flatMap[B](f: A => Task[B]): Task[B] = ???

      def attempt: Task[Try[A]] = ???
    }

    override def raiseError[A](err: Throwable): Task[A] = ???
  }
}
