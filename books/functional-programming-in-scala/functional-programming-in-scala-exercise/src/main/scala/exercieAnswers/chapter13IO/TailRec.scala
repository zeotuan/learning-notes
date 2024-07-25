package exercieAnswers.chapter13IO

import scala.annotation.tailrec

object TailRecIO {
  /**
   * General purpose data type for optimizing tail calls.
   * This data type is also sometimes called `Trampoline` Because
   * The way interpreting it bounce back and forth between the main
   * [[run]] loop and the functions contained in the `TailRec`
   *
   * TailRec is slower than direct function call but has predictable stack usage
   * This data type is built into the standard library under [[scala.util.control.TailCalls]]
   * */

  trait TailRec[A] {
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(this, f)
    def map[B](f: A => B): TailRec[B] = flatMap(f andThen(Return(_)))

    @tailrec
    final def run(): A = this match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) => x match {
        case Return(a) => f(a).run()
        case Suspend(r) => f(r()).run()
        // This is nested FlatMap constructor on the left FlatMap(FlatMap(y, g), f)
        // Since this can be of arbitrary depth, we re-associate it to the right effectively
        // Turning y.flatMap(g).flatMap(f) => y.flatMap(a => g(a).flatMap(f))
        // due to monad associativity law this two are equivalent and it allow us to remain
        // tail recursive. When the program is interpreted, it will be incrementally be rewritten:
        // FlatMap(a1, a1 =>
        //  FlatMap(a2, a2 =>
        //    FlatMap(a3, a3 =>
        //      ...
        //      FlatMap(aN, aN => Return(aN)))))
        case FlatMap(y, g) => y.flatMap(a => g(a).flatMap(f)).run()
      }
    }
  }

  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class Return[A](a: A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B]) extends TailRec[B]

  object TailRec extends Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = Return(a)
    override def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a flatMap f
    def suspend[A](a: => TailRec[A]): TailRec[A] = Suspend(() => ()).flatMap(_ => a)
  }
}
