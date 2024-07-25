package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par

import scala.annotation.tailrec

object AsyncTailRec {
  /**
   * Previous [[TailRec]] implementation is still inexplicit about
   * what effects can occur. It is also always block the thread
   * without any support for async execution due to Suspend thunks
   * blocking whenever it run. This can simply be fixed by making Suspend
   * constructor to accept a Par instead of thunk
   * TailRec Implementation which support Async execution */
  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] = FlatMap(this, f)
    def map[B](f: A => B): Async[B] = flatMap(a => Return(f(a)))

    @tailrec
    final def step: Async[A] = this match {
      case FlatMap(FlatMap(x, f), g) => x.flatMap(a => f(a).flatMap(g)).step
      case FlatMap(Return(x), f) => f(x).step
      case _ => this
    }

    /**
     * Suspend and Return only use unit and flatMap only using unit and flatMap
     * indicate That This data structure can be generalize even more to
     * accept any Monad. We'll cal this [[Free]]
     * */
    def run: Par[A] = step match {
      case Return(a) => Par.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => Par.flatMap(r)(a => f(a).run)
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }
    }
  }

  object Async extends Monad[Async] {
    def unit[A](a: => A): Async[A] = Return(a)
    def flatMap[A, B](a: Async[A])(f: A => Async[B]): Async[B] = a flatMap f
  }

  case class Return[A](a: A) extends Async[A]
  case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]
}

