package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par
import exercieAnswers.chapter13IO.IOApp.{IO, par, unsafePerformIO}

import java.util.concurrent.ExecutorService
import scala.util.{Failure, Success, Try}

/*
 * `Task[A]` is a wrapper around `Free[Par, Try[A]]`, with some
 * convenience functions for handling exceptions.
 */
case class Task[A](get: IO[Try[A]]) {
  def flatMap[B](f: A => Task[B]): Task[B] = Task(get.flatMap {
    case Failure(e) => IO(Failure(e))
    case Success(a) => f(a).get
  })

  def map[B](f: A => B): Task[B] = flatMap(f andThen Task.now)

  def attempt: Task[Try[A]] =
    Task(get map {
      case Failure(e) => Success(Failure(e))
      case Success(a) => Success(Success(a))
    })

  def handle[B >: A](f: PartialFunction[Throwable, B]): Task[B] =
    attempt flatMap {
      case Failure(e) => f.lift(e) map Task.now getOrElse Task.fail(e)
      case Success(a) => Task.now(a)
    }

  def or[B >: A](t2: Task[B]): Task[B] =
    Task(this.get flatMap {
      case Failure(_) => t2.get
      case a => IO(a)
    })

  def run(implicit E: ExecutorService): A = unsafePerformIO(get) match {
    case Failure(e) => throw e
    case Success(a) => a
  }

  def attemptRun(implicit E: ExecutorService): Try[A] =
    try unsafePerformIO(get) catch {
      case t: Throwable => Failure(t)
    }
}

object Task extends Monad[Task] {
  def unit[A](a: => A) = Task(IO(Try(a)))

  def flatMap[A, B](a: Task[A])(f: A => Task[B]): Task[B] = a flatMap f

  def fail[A](e: Throwable): Task[A] = Task(IO(Failure(e)))

  def now[A](a: A): Task[A] = Task(Return(Success(a)))

  def more[A](a: => Task[A]): Task[A] = Task.now(()) flatMap (_ => a)

  def delay[A](a: => A): Task[A] = more(now(a))

  def fork[A](a: => Task[A]): Task[A] =
    Task {
      par {
        Par.lazyUnit(())
      } flatMap (_ => a.get)
    }

  def forkUnit[A](a: => A): Task[A] = fork(now(a))

  def Try[A](a: => A): Either[Throwable, A] =
    try Right(a) catch {
      case e: Throwable => Left(e)
    }
}

