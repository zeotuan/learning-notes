package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par
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
}
