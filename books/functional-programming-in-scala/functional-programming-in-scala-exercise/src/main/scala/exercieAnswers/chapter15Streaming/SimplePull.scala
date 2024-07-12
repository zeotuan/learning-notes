package exercieAnswers.chapter15Streaming

import Pull._
import exercieAnswers.chapter10Monoid.Monoid
import exercieAnswers.chapter13IO.Monad

import scala.annotation.tailrec

/**
 * output any number of values of type [[O]] and then terminates with a single value of type [[R]].
 * Can be incrementally evaluated via [[step]], returning either:
 * - a final value [[R]]
 * - a value [[O]] with a new [[Pull]] representing the remainder of the stream
 * This model allow constructing arbitrarily complex pull by from various source by sequencing [[Output]] call using
 * [[flatMap]]. when final transformation is needed, we can use [[fold]] which repeatedly [[step]] until
 * termination
 * */
trait Pull[+O, +R] {

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

  /** R type is Nothing since this pull is infinite and never terminate*/
  def repeat: Pull[O, Nothing] = this >> repeat

  /**
   * eagerly and partially evaluate [[Pull]] until specified number of output values has been generated
   * */
  def takeEagerly(n: Int): Pull[O, Option[R]] = if (n <= 0)
    Result(None)
  else
    step(this) match {
      case Left(r) => Result(Some(r))
      case Right((hd, tl)) => Output(hd) >> tl.takeEagerly(n - 1)
    }

  /** wraps step in a [[Result]] constructor and delays it's creation via flatMap ([[>>]]) */
  def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] = done >> Result(step(this))

  /**
   * partially  evaluate [[Pull]] until specified number of output values has been generated
   * */
  def take(n: Int): Pull[O, Option[R]] = if (n <= 0)
    Result(None)
  else
    uncons.flatMap {
      case Left(r) => Result(Some(r))
      case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
    }

  /** Exercise 15.3: implement drop, takeWhile, dropWhile */
  def drop(n: Int): Pull[O, R] = if (n <= 0)
    this
  else
    uncons.flatMap {
      case Left(r) => Result(r)
      case Right((_, tl)) => tl.drop(n - 1)
    }

  def takeWhile(f: O => Boolean): Pull[O, Pull[O, R]] = uncons.flatMap {
      case Left(r)                  => Result(Result(r))
      case Right((hd, tl)) if f(hd) => Output(hd) >> tl.takeWhile(f)
      /** Return the remaining stream as [[Result]] as we are not suppsoed to terminate */
      case Right((hd, tl))          => Result(Output(hd) >> tl)
    }

  def dropWhile(p: O => Boolean): Pull[Nothing, Pull[O, R]] = uncons.flatMap {
    case Left(r)                  => Result(Result(r))
    case Right((hd, tl)) if p(hd) => tl.dropWhile(p)
    case Right((hd, tl))          => Result(Output(hd) >> tl)
  }

  def mapOutput[O2](f: O => O2): Pull[O2, R] = uncons.flatMap {
    case Left(r) => Result(r)
    case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
  }

  def filter(p: O => Boolean): Pull[O, R] = uncons.flatMap {
    case Left(r) => Result(r)
    case Right((hd, tl)) => (if (p(hd)) Output(hd) else done) >> tl.filter(p)
  }

  def count: Pull[Int, R] = {
    def go(total: Int, p: Pull[O, R]): Pull[Int, R] = p.uncons.flatMap {
      case Left(r) => Result(r)
      case Right((_, tl)) =>
        val newTotal = total + 1
        Output(newTotal) >> go(newTotal, tl)
    }
    go(0, this)
  }

  def tally[O2 >: O](implicit m: Monoid[O2]): Pull[O2, R] = {
    def go(curr: O2, p: Pull[O, R]): Pull[O2, R] = p.uncons.flatMap {
      case Left(r) => Result(r)
      case Right((hd, tl)) =>
        val newCurr = m.combine(curr, hd)
        Output(newCurr) >> go(newCurr, tl)
    }
    Output(m.empty) >> go(m.empty, this)
  }

  def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[O2, (S, R)] = uncons.flatMap {
    case Left(r)         => Result((init, r))
    case Right((hd, tl)) =>
      val (s, out) = f(init, hd)
      Output(out) >> tl.mapAccumulate(s)(f)
  }

  /** Exercise 15.6: Implement count, tally, slidingMean via mapAccumulate */
  def countViaMapAccumulate: Pull[Int, R] = Output(0) >> mapAccumulate(0)((s, o) => (s + 1, s + 1)).map(_._2)
  def tallyViaMapAccumulate[O2 >: O](implicit m: Monoid[O2]): Pull[O2, R] = Output(m.empty) >> mapAccumulate(m.empty) { (s, o) =>
    val s2 = m.combine(s, o)
    (s2, s2)
  }.map(_._2)
}
case class Result[+R](result: R) extends Pull[Nothing, R]
case class Output[+O](value: O) extends Pull[O, Unit]
case class FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

object Pull {
  /**
   * interpret pull while rewriting left nested flatmap call
   * */
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

  def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[O, R] = f(init) match {
    case Left(r) => Result(r)
    case Right((o, r2)) => Output(o) >> unfold(r2)(f)
  }

  /** 15.1: LazyList and List via unfold */
  def fromListViaUnfold[O](os: List[O]): Pull[O, Unit] = unfold(os) {
    case Nil => Left(Nil)
    case h::t => Right((h, t))
  }.map(_ => ())

  def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[O, Unit] = unfold(os) {
    case LazyList() => Left(LazyList())
    case h #::t => Right((h, t))
  }.map(_ => ())

  def continually[A](a: A): Pull[A, Nothing] = Output(a) >> continually(a)

  def continuallyViaRepeat[A](a: A): Pull[A, Nothing] = Output(a).repeat

  /**
   * return an infinite stream with first output as the `initial` value
   * and subsequent output as result of applying `f`
   * */
  def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] = Output(initial) >> iterate(f(initial))(f)

  /** 15.5:  */
  def slidingMean[R](p: Pull[Int, R])(n: Int): Pull[Double, R] = {
    def go(window: collection.immutable.Queue[Int], p: Pull[Int, R]): Pull[Double, R] = p.uncons.flatMap {
      case Left(r) => Result(r)
      case Right((hd, tl)) =>
        val newWindow = if (window.size < n) window :+ hd else window.tail :+ hd
        val newWindowMean = newWindow.sum / newWindow.size.toDouble
        Output(newWindowMean) >> go(newWindow, tl)
    }
    go(collection.immutable.Queue.empty, p)
  }


  def slidingMeanViaMapAccumulate[R](p: Pull[Int, R])(n: Int): Pull[Double, R] = p.mapAccumulate(collection.immutable.Queue.empty[Int]) {
    (window, o) =>
      val newWindow = if (window.size < n ) window :+ o else window.tail :+ o
      val newWindowMean = newWindow.sum / newWindow.size.toDouble
      (newWindow, newWindowMean)
  }.map(_._2)

  /** flatMap with Result constructor form a monad instance for `[x] =>> Pull[O,  x]`*/
  implicit def monad[O]: Monad[({ type p[x] = Pull[O, x] })#p] = new [({ type p[x] = Pull[O, x] })#p] {
    def unit[A](a: => A): Pull[O, A] = Result(a)
    def flatMap[A, B](p: Pull[O, A])(f: A => Pull[O, B]): Pull[O, B] = p.flatMap(f)
  }

  /**
   * Alternative flatMap for Monad which take Output as the implementation of [[Monad.unit]]
   * This Monad implementation is more similar to monad for [[List]] and [[LazyList]] which
   * the supplied information is invoke for the [[Output]] element and result are concatenated
   * */
  val OutputMonad: Monad[({ type p[x] = Pull[x, Unit] })#p] = new [({ type p[x] = Pull[x, Unit] })#p] {
    def unit[A](a: => A): Pull[A, Unit] = Output(a)
    def flatMap[A, B](p: Pull[A, Unit])(f: A => Pull[B, Unit]): Pull[B, Unit] = flatMapOutput(p)(f)
  }
  def flatMapOutput[O, O2](p: Pull[O, Unit])(f: O => Pull[O2, Unit]): Pull[O2, Unit] = p.uncons.flatMap {
    case Left(())        => Result(())
    case Right((hd, tl)) => f(hd) >> flatMapOutput(tl)(f)
  }


}

object PullExamnple {
  // Example 1
  val p = Output(1) >> Output(2) // FlatMap(Output(1), _ => Output(2))
  val q = p.toList // List(1,2)

  // Example 2
  val ints = Pull.iterate(0)(_ + 1) // FlatMap(Output(0), _ => Output(0 + 1), ....)
  val firstFive = ints.take(5).toList // List(0, 1, 2, 3, 4)

}