package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter13IO.FreeTest.Console.IO

import scala.annotation.tailrec


trait Pull[+O, +R] {
  import Pull._

  /** Operates on the final result value of a pull, not each output value */
  def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] = FlatMap(this, f)

  /** Steps the pull until a final result is produced, accumulating an output value */
  @tailrec
  final def fold[A](init: A)(f: (A, O) => A): (R, A) = step(this) match {
    case Left(r)         => (r, init)
    case Right((hd, tl)) => tl.fold(f(init, hd))(f)
  }

  /** fold the pull, collecting all output element and discard result R */
  def toList: List[O] = fold(List.newBuilder[O])((acc, o) => acc += o)._2.result()

  def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] = flatMap(_ => next)

  def map[R2](f: R => R2): Pull[O, R2] = flatMap(r => Result(f(r)))
}
case class Result[+R](result: R) extends Pull[Nothing, R]
case class Output[+O](value: O) extends Pull[O, Unit]
case class FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

object Pull {
  /** interpret pull while rewriting left nested flatmap call */
  def step[O, R](pull: Pull[O, R]): Either[R, (O, Pull[O, R])] = pull match {
    case Result(r) => Left(r)
    case Output(o: O) => Right(o, Pull.done)
    case FlatMap(source, f) => source match {
      case FlatMap(s2, g) => step(s2.flatMap(x => g(x).flatMap(f)))
      case other => step(other) match {
        case Left(r) => step(f(r))
        case Right((hd, tl)) => Right((hd, tl.flatMap(f)))
      }
    }
  }

  def done: Pull[Nothing, Unit] = Result(())

  def fromList[O](os: List[O]): Pull[O, Unit] = os match {
    case Nil => done
    case h::t => Output(h) >> fromList(t)
  }

  def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] = os match {
    case LazyList() => done
    case h #:: t => Output(h) >> fromLazyList(t)
  }
}

object PullExamnple {
  val p = Output(1) >> Output(2) // FlatMap(Output(1), _ => Output(2))
  val q = p.toList // List(1,2)
}