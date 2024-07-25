package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par
import exercieAnswers.chapter11Monad.Monad
import exercieAnswers.chapter11Monad.Monad.parMonad
import exercieAnswers.chapter13IO.Free._
import exercieAnswers.chapter13IO.FreeTest.Console.ParMonad
import exercieAnswers.chapter13IO.IOApp.IO

import java.util.concurrent.{ExecutorService, Executors}

trait IOApp {
  def unsafeRunSync[A](ioa: IO[A])(pool: ExecutorService): A = Par.run(pool)(ioa.run(ParMonad))

  def pureMain(args: Seq[String]): IO[Unit]

  def main(args: Array[String]): Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafeRunSync(pureMain(args))(pool)

  }
}

object IOApp {
  type IO[A] = FreeTest.Console.IO[A]

  def IO[A](a: => A): IO[A] = FreeTest.Console.IO[A](a)

  implicit val ioMonad = freeMonad[Par]

  def now[A](a: A): IO[A] = Return(a)

  def fork[A](a: => IO[A]): IO[A] = par(Par.lazyUnit(())) flatMap (_ => a)

  def forkUnit[A](a: => A): IO[A] = fork(now(a))

  def delay[A](a: => A): IO[A] = now(()) flatMap (_ => now(a))

  def par[A](a: Par[A]): IO[A] = exercieAnswers.chapter13IO.Suspend(a)

  def async[A](cb: ((A => Unit) => Unit)): IO[A] =
    fork(par(Par.async(cb)))

  def Return[A](a: A): Return[Par, A] = exercieAnswers.chapter13IO.Return[Par, A](a)

  import java.util.concurrent.ExecutorService
  def unsafePerformIO[A](io: IO[A])(implicit E: ExecutorService): A =
    Par.run(E) { io.run(FreeTest.Console.parMonad) }
}
