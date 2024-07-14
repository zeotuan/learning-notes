package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter10Monoid.Monoid
import exercieAnswers.chapter13IO.Monad
import exercieAnswers.chapter13IO.FreeTest.Console.IO

import java.nio.file.{Files, Paths}
import scala.annotation.tailrec

object SimplePull {
  import Pull._
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
      /** Return the remaining stream as [[Result]] as we are not supposed to terminate */
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

    /** ResultMonad: flatMap with Result constructor form a monad instance for `[x] =>> Pull[O,  x]`*/
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

    implicit class PullOps[O](self: Pull[O, Unit]) {
      def toStream: Stream[O] = self
    }

    /**
     * Pull let us describe arbitrarily complex data sources in terms of monadic recursion
     * Stream us collection like API that operates on individual element output by Pull
     *
     * In scala 3 we would be able to bind flatMapOutPut and mapOutput to flatMap and Map
     * of Stream using opaque type and given. THis might be possible to create a wrapper around
     * Pull but we will need to reimplement a lot of wrapper for pull implementation
     * */
    type Stream[+O] = Pull[O, Unit]
    object Stream extends Monad[Stream]{
      def apply[O](os: O*): Stream[O] = Pull.fromList(os.toList)

      implicit class StreamOps[O](self: Stream[O]) {
        def toPull: Pull[O, Unit] = self

        def fold[A](init: A)(f: (A, O) => A):  A = self.fold(init)(f)._2

        def toList: List[O] = self.toList

        def take(n: Int): Stream[O] =  monad.void(self.take(n)) // self.take(n).map(_ => ())

        def ++(that: => Stream[O]): Stream[O] = self >> that
      }
      def unit[A](a: => A): Stream[A] = Output(a).toStream
      def flatMap[A, B](a: Stream[A])(f: A => Stream[B]): Stream[B] = flatMapOutput(a)(f)
    }

    type Pipe[-I, O] = Stream[I] => Stream[O]
  }

  object PullExamnple {
    // Example 1
    val p = Output(1) >> Output(2) // FlatMap(Output(1), _ => Output(2))
    val q = p.toList // List(1,2)

    // Example 2
    val ints = Pull.iterate(0)(_ + 1) // FlatMap(Output(0), _ => Output(0 + 1), ....)
    val firstFive = ints.take(5).toList // List(0, 1, 2, 3, 4)

    val nonEmpty: Pipe[String, String] = _.filter(_.nonEmpty)

    val lowerCase: Pipe[String, String] = _.mapOutput(_.toLowerCase())

    val normalize: Pipe[String, String] = nonEmpty andThen lowerCase

    val lines: Stream[String] = Stream("Hello", " ", "World!")
    val normalized: Stream[String] = normalize(lines)

    import scala.util.chaining.scalaUtilChainingOps
    val normalized2: Stream[String] = lines.pipe(normalize)
    val normalized3: Stream[String] = lines.pipe(nonEmpty).pipe(lowerCase)

    /** 5.17: Non halting version which map all element to boolean and apply logical Or on each subsequent element */
    def exists[I](f: I => Boolean): Pipe[I, Boolean] = _.mapOutput(o => f(o)).tally(Monoid.booleanOr)


    def takeThrough[I](f: I => Boolean): Pipe[I, I] = src => monad.void(src.takeWhile(f).flatMap(_.take(1)))

    def dropWhileStream[I](f: I => Boolean): Pipe[I, I] = src => monad.void(src.dropWhile(f).flatMap(identity))

    /** We can use [[takeThrough]] and [[dropWhileStream]] to implement the halting version of exist */
    def existHalting[I](f: I => Boolean): Pipe[I, Boolean] =  exists(f) andThen takeThrough(!_) andThen dropWhileStream(!_)

    /** output the last value of a pull */
    def last[I](init: I): Pipe[I, I] = {
      def go(value:I, p: Stream[I]): Stream[I] = p.uncons.flatMap {
        case Left(_) => Output(value)
        case Right((hd, tl)) => go(hd, tl)
      }
      src => go(init, src)
    }

    /** use [[last]] instead of [[dropWhileStream]] to implement exist halting */
    def existHalting2[I](f: I => Boolean): Pipe[I, Boolean] =  exists(f) andThen takeThrough(!_) andThen last(false)

    def count[I]: Pipe[I, Int] = src => monad.void(src.count)

    def countGt40k[I]: Pipe[I, Boolean] = count andThen existHalting(_ > 400000)

    def fromIterator[O](itr: Iterator[O]): Stream[O] = monad.void(unfold(itr)(it => if (it.hasNext) Right(it.next() -> it) else Left(itr)))

    def processFile[A](file: java.io.File, p: Pipe[String, A])(implicit M: Monoid[A]): IO[A] = IO {
      val source = scala.io.Source.fromFile(file)
      try {
        fromIterator(source.getLines).pipe(p).fold(M.empty)(M.combine)._2
      } finally {
        source.close()
      }
    }

    def checkFileForGt40K(file: java.io.File): IO[Boolean] = processFile(file, countGt40k)(Monoid.booleanOr)

    def toCelsius(fahrenheit: Double): Double = (5.0 / 9.0) * (fahrenheit - 32.0)

    def trimmed: Pipe[String, String] = _.mapOutput(_.trim)

    def skipComment: Pipe[String, String] = _.filter(s => s.charAt(0) != '#')

    def toDouble: Pipe[String, Double] = src => flatMapOutput(src)(_.toDoubleOption.fold(Stream())(Stream(_)))

    def convertToCelsius: Pipe[Double, Double] = src => src.mapOutput(toCelsius)

    val stringToCelsius: Pipe[String, Double] =
      trimmed andThen
        nonEmpty andThen
        skipComment andThen
        toDouble andThen
        convertToCelsius


    def convert(inputFile: String, outputFile: String): IO[Unit] = IO {
      val source = scala.io.Source.fromFile(inputFile)
      try {
        val writer = Files.newBufferedWriter(Paths.get(outputFile))
        try {
          fromIterator(source.getLines()).pipe(stringToCelsius).fold(()) { (_ , a) =>
            writer.write(a.toString)
            writer.newLine()
          }
        } finally {
          writer.close()
        }
      } finally {
        source.close()
      }
    }
  }
}

