package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par
import exercieAnswers.chapter13IO.Free.{freeMonad, runTrampoline}

import scala.annotation.tailrec
import scala.io.StdIn.readLine
import scala.util.Try

/**
 * A generalize version of [[Async]] and [[TailRec]]
 * which accept a [[Monad]] for any choice  of [[F]].
 *
 * In [[TailRec]] [[F]] is parameterized to a [[Function0]]
 *
 * In [[Async]] [[F]] is parameterized to a [[exercieAnswers.chapter07Parallelism.NonBlockingPar.Par]]
 *
 * Essentially Free is a recursive strcture that contains a value of Type [[A]]
 * wrapped in zero or more layers of [[F]]. It's a [[Monad]] since it allow us to
 * take [[A]] and from  it generate more layers of [[F]]. The [[run]] is the interpreter
 * and it must be able to process all of those [[F]] layer before getting the result.
 * The structure and it's interpreter can be viewed as interacting coroutines where [[F]]
 * define the protocol of the interaction.
 * */
sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =  FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen(Return(_)))

  /**
   * Converting input into one of three cases:
   *
   * - [[Return]]
   *
   * - [[Suspend]]
   *
   * - Right associated `FlatMap(Suspend(r), f)`
   * */
  @tailrec
  final def step: Free[F, A] = this match {
    case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g))
    case FlatMap(Return(x), f) => f(x).step
    case _ => this
  }


  def run(implicit FM: Monad[F]): F[A] = step match {
    case Return(a) => FM.unit(a)
    case Suspend(r) => r
    case FlatMap(Suspend(r), f) => FM.flatMap(r)(a => f(a).run)
    case _ => sys.error("Impossible as step has eliminated all other cases")
  }
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B]) extends Free[F, B]

object Free {
  /** 13.1: implement freeMonad */
  def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({ type f[a] = Free[F, a] })#f] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa flatMap f
  }

  /** 13.2:
   * Implement specialized runTrampoline interpreter
   * similar to the one used by [[TailRec]]
   * */
  def runTrampoline[A](a: Free[Function0,A]): A = a match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => runTrampoline(f(a))
      case Suspend(r) => runTrampoline(f(r()))
      case FlatMap(y, g) => runTrampoline(y.flatMap(a => g(a).flatMap(f)))
    }
  }
}

object FreeTest {
  /** ConsoleIO using Free */

  sealed trait Console[A] {
    /** Interpret [[Console]] as [[Par]] */
    def toPar: Par[A]

    /** Interpret [[Console]] as [[Function0]] */
    def toThunk: () => A
  }

  case class ReadLine() extends Console[Option[String]] {
    override def toPar: Par[Option[String]] = Par.lazyUnit(Try(readLine()).toOption)

    override def toThunk: () => Option[String] = () => Try(readLine()).toOption
  }
  case class PrintLine(line: String) extends Console[Unit] {
    override def toPar: Par[Unit] = Par.lazyUnit(println(line))

    override def toThunk: () => Unit = () => println(line)
  }

  object Console {
    def readLn: Free[Console, Option[String]] = Suspend(ReadLine)
    def printLn(line: String): Free[Console, Unit] = Suspend(PrintLine(line))

    /**
     * To run a [[Console]] Program, we need a Monad[Console] but since it's impossible to implement flatMap
     * for [[Console]], we need a way to translate our Console type to other supported type(either [[Function0]] or [[Par]]).
     * We introduce the following type to do this translation:
    */

    /** Translate between any F[A] to G[A] */
    trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
    /** infix syntax F ~> G for Translate[F,G] */
    type ~>[F[_], G[_]] = Translate[F, G]

    val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]): () => A = a.toThunk }
    val consoleToPar = new (Console ~> Par) { def apply[A](a: Console[A]): Par[A] = a.toPar }

    def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G) (implicit G: Monad[G]): G[A] = free.step match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible as step has eliminated all other cases")
    }

    implicit val function0Monad = new Monad[Function0] {
      override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () => f(a())()
      override def unit[A](a: => A): () => A = () => a
    }

    implicit val ParMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)
      override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }
    }

    def runConsoleFunction0[A](a: Free[Console, A]): () => A = runFree[Console, Function0, A](a)(consoleToFunction0)
    def runConsolePar[A](a: Free[Console, A]): Par[A] = runFree[Console, Par, A](a)(consoleToPar)

    /**
     * The [[runConsoleFunction0]] implementation is unfortunately not stack safe,
     * because it relies of the stack safety of the underlying monad, and the
     * [[Function0]] monad we gave is not stack safe.
     *
     * 13.4: Implement runConsole using runFree:
     * Previously we were converting from Free[F,A] into H[A] for some constructor H
     * However this did not make use of Free which make runFree unsafe if flatMap implementation is not stackSafe
     * If we take H[A] = Free[G, A], we meed polymorphic function to convert from F[A] to Free[G,A]
     * Which can be implemented by Suspending the  result of translating F[A] to G[A]
     * To fix this, we must convert from Free[F, A] => Free[G, A]
     *
     * This allow use to make use of [[freeMonad]] instead and keep runConsole stacksafe
     */
    def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
      type H[A] = Free[G,A]
      val t = new (F ~> H) {
        def apply[A](a: F[A]): Free[G,A] = Suspend { fg(a) }
      }
      runFree(f)(t)(freeMonad[G])
    }

    def refinedRunConsoleFunction0[A](a: Free[Console, A]): A = runTrampoline(translate(a)(consoleToFunction0))

    /**
     * Free[Console, A] doesn't require us to interpret Console using side effects.
     * The decision is entirely responsibility of the interpreter.
     * We can choose an interpreter which produce a side effect or not:
     *  */

    case class ConsoleReader[A](run: String => A) {
      def map[B](f: A => B): ConsoleReader[B] = ConsoleReader(run andThen f)
      def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =  ConsoleReader(r => f(run(r)).run(r))
    }

    object ConsoleReader {
      implicit val monad = new Monad[ConsoleReader] {
        override def unit[A](a: => A): ConsoleReader[A] = ConsoleReader(_ => a)
        override def flatMap[A, B](a: ConsoleReader[A])(f: A => ConsoleReader[B]): ConsoleReader[B] = a flatMap f
      }
    }

  }
}
