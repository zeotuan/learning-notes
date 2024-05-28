package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par

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
}
