package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter10Monoid.Monoid
import exercieAnswers.chapter13IO.{Monad, Task}
import exercieAnswers.chapter13IO.IOApp.IO
import exercieAnswers.chapter15Streaming.ErrorHandlingPull.Pull

import java.nio.file.{Files, Paths}
import java.time.Instant
import java.util.UUID
import scala.Stream
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Try

object ErrorHandlingPull {
  import Pull._
  trait Pull[+F[_], +O, +R] {
    def flatMap[F2[x] >: F[x], O2 >: O, R2](f: R => Pull[F2, O2, R2]): Pull[F2, O2, R2] = FlatMap(this, f)
    def step[F2[x] >: F[x], O2 >: O, R2 >: R](implicit F: MonadThrow[F2]): F2[Either[R2, (O2, Pull[F2, O2, R2])]] = this match {
      case Result(r) => F.unit(Left(r)) // use Monad instance for Effect type to lift result into effect type
      case Output(o: O) => F.unit(Right(o, Pull.done)) // use Monad instance for Effect type to lift result into effect type
      case Eval(action) => F.map(action)(Left(_)) // map result of action in a Left
      case Uncons(source) => F.map(source.step(F))(s => Left(s.asInstanceOf[R2]))
      case Handle(source, f) => source match {
        case Handle(s2, g) => s2.handleErrorWith(x => g(x).handleErrorWith(y => f(y))).step // Rewrite left nested handler to right nested handler
        case other =>
          import F.MonadThrowOps
          F.map(other.step(F)) {
            case Right((hd, tl)) => Right((hd, Handle(tl, f)))
            case l@Left(_) => l
          }.handleErrorWith(t => f(t).step) // handle error occur when stepping source pull
      }
      case FlatMap(source, f) => source match {
        case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(f)).step
        case other => F.flatMap(other.step(F)) { // recursively step using flatMap to access result of first step
          case Left(r) => f(r).step
          case Right((hd, tl)) => F.unit(Right((hd, tl.flatMap(f))))
        }
      }
    }

    def handleErrorWith[F2[x] >: F[x], O2 >: O, R2 >: R](handler: Throwable => Pull[F2, O2, R2]): Handle[F2, O2, R2] = Handle(this, handler)

    /** similar to [[step]], fold is no longer tail recursive and stack-safety is depend on Effect Type Monad */
    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A) (implicit F: MonadThrow[F2]): F2[(R2, A)] = F.flatMap(step(F)) {
      case Left(r) => F.unit((r, init))
      case Right((hd, tl)) => tl.fold[F2, R2, A](f(init, hd))(f)(F)
    }

    def toList[F2[x] >: F[x]: Monad, O2 >: O](implicit F: MonadThrow[F2]): F2[List[O2]] = F.map(fold[F2, R, mutable.Builder[O2, List[O2]]](List.newBuilder[O2])((acc, o) => acc += o)(F))(w => w._2.result())

    /**
     * because step force monad constraint on the effect type, we can no longer call uncons on a non effectful pull (F = Nothing)
     * To battle this, we can defer the invocation of [[step]] until the resultant pull is Evaluated
     * We will introduce a new [[Uncons]] constructor for this.
     *
     * wraps step in a [[Result]] constructor and delays it's creation via flatMap ([[>>]])
     *  */
    def uncons: Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]] = Uncons(this)

    def >>[F2[x] >: F[x], O2 >: O, R2](next: => Pull[F2, O2, R2]): Pull[F2, O2, R2] = flatMap(_ => next)

    def map[R2](f: R => R2): Pull[F, O, R2] = flatMap(r => Result(f(r)))

    def repeat: Pull[F, O, Nothing] = this >> repeat

    def take(n: Int): Pull[F, O, Option[R]] = if (n <= 0)
      Result(None)
    else
      uncons.flatMap {
        case Left(r) => Result(Some(r))
        case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)
      }

    def drop(n: Int): Pull[F, O, R] = if (n <= 0)
      this
    else
      uncons.flatMap {
        case Left(r) => Result(r)
        case Right((_, tl)) => tl.drop(n - 1)
      }

    def takeWhile(f: O => Boolean): Pull[F, O, Pull[F, O, R]] = uncons.flatMap {
      case Left(r) => Result(Result(r))
      case Right((hd, tl)) if f(hd) =>  Output(hd) >> tl.takeWhile(f)
      /** Return the remaining stream as [[Result]] as we are not supposed to terminate */
      case Right((hd, tl)) => Result(Output(hd) >> tl)
    }


    def dropWhile(p: O => Boolean): Pull[F, Nothing, Pull[F, O, R]] = uncons.flatMap {
      case Left(r) => Result(Result(r))
      case Right((hd, tl)) if p(hd) => tl.dropWhile(p)
      /** Return the remaining stream as [[Result]] as we are not supposed to terminate */
      case Right((hd, tl)) => Result(Output(hd) >> tl)
    }

    def mapOutput[O2](f: O => O2): Pull[F, O2, R] = uncons.flatMap {
      case Left(r) => Result(r)
      case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)
    }

    def filter(p: O => Boolean): Pull[F, O, R] = uncons.flatMap {
      case Left(r) => Result(r)
      case Right((hd, tl)) => (if (p(hd)) Output(hd) else done) >> tl.filter(p)
    }


    def tally[O2 >: O](implicit m: Monoid[O2]): Pull[F, O2, R] = {
      def go(curr: O2, p: Pull[F, O, R]): Pull[F, O2, R] = p.uncons.flatMap {
        case Left(r) => Result(r)
        case Right((hd, tl)) =>
          val newCurr = m.combine(curr, hd)
          Output(newCurr) >> tl.tally(m)
      }
      go(m.empty, this)
    }

    def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[F, O2, (S, R)] = uncons.flatMap {
      case Left(r) => Result((init, r))
      case Right((hd, tl)) =>
        val (s, o) = f(init, hd)
        Output(o) >> tl.mapAccumulate(s)(f)
    }

    def tallyViaMapAccumulate[O2 >: O](implicit m: Monoid[O2]): Pull[F, O2, R] = Output(m.empty) >> mapAccumulate(m.empty) { (s, o) =>
      val s2 = m.combine(s, o)
      (s2, s2)
    }.map(_._2)
  }
  case class Result[+R](result: R) extends Pull[Nothing, Nothing, R]
  case class Output[+O](value: O) extends Pull[Nothing, O, Unit]
  case class Eval[+F[_], R](action: F[R]) extends Pull[F, Nothing, R]
  case class FlatMap[+F[_], X, +O, +R](source: Pull[F, O, X], f: X => Pull[F, O, R]) extends Pull[F, O, R]
  case class Uncons[+F[_], +O, +R](source: Pull[F, O, R]) extends Pull[F, Nothing, Either[R, (O, Pull[F, O, R])]]
  case class Handle[+F[_], +O, +R](source: Pull[F, O ,R], handler: Throwable => Pull[F, O, R]) extends Pull[F, O, R]
  case class Error(e: Throwable) extends Pull[Nothing, Nothing, Nothing]


  object Pull {
    def done: Pull[Nothing, Nothing, Unit] = Result(())

    def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[Nothing, O, R]  = f(init) match {
      case Left(r) => Result(r)
      case Right((o, r2)) => Output(o) >> unfold(r2)(f)
    }

    implicit def resultMonad[F[_], O]: Monad[({ type p[x] = Pull[F, O, x] })#p] = new [({ type p[x] = Pull[F, O, x] })#p] {
      def unit[A](a: => A): Pull[F, O, A] = Result(a)
      def flatMap[A, B](p: Pull[F, O, A])(f: A => Pull[F, O, B]): Pull[F,O, B] = p.flatMap(f)
    }

    implicit def OutputMonad[F[_]]: Monad[({ type p[x] = Pull[F, x, Unit] })#p] = new [({ type p[x] = Pull[F, x, Unit] })#p] {
      def unit[A](a: => A): Pull[F, A, Unit] = Output(a)
      def flatMap[A, B](p: Pull[F, A, Unit])(f: A => Pull[F, B, Unit]): Pull[F,  B, Unit] = flatMapOutput(p)(f)
    }

    def flatMapOutput[F[_], O, O2](p: Pull[F, O, Unit])(f: O => Pull[F, O2, Unit]): Pull[F, O2, Unit] = p.uncons.flatMap {
      case Left(())        => Result(())
      case Right((hd, tl)) => f(hd) >> flatMapOutput(tl)(f)
    }

    def fromList[F[_], O](os: List[O]): Pull[F, O, Unit] = os match {
      case Nil => done
      case h::t => Output(h) >> fromList(t)
    }


    def unfoldEval[F[_], O, R](init: R)(f: R => F[Either[R, (O, R)]]): Pull[F, O, R] = Eval(f(init)).flatMap {
        case Left(r) => Result(r)
        case Right((o, r2)) => Output(o) >> unfoldEval(r2)(f)
      }
  }

  type Stream[+F[_], +O] = Pull[F, O, Unit]

  object Stream {
    def empty: Stream[Nothing,  Nothing] = Pull.done
    def apply[O](os: O*): Stream[Nothing, O] = fromList(os.toList)

    def eval[F[_], O](fo: F[O]): Stream[F, O] = Eval(fo).flatMap(Output(_))

    def unfoldEval[F[_], O, R](init: R)(f: R => F[Option[(O, R)]]): Stream[F, O] = Eval(f(init)).flatMap {
      case None => empty
      case Some((o, r)) => Output(o) >> unfoldEval(r)(f)
    }

    def fromIterator[O](itr: Iterator[O]): Stream[Nothing, O] =
      if (itr.hasNext) Output(itr.next) >> fromIterator(itr) else Pull.done

    def raiseError[F[_], O](t: Throwable): Stream[F, O] = Error(t)

    implicit class StreamOps[F[_], O](self: Stream[F, O]) {
      def toPull: Pull[F, O, Unit] = self

      /** fold and toList are eliminator of Stream type */

      def fold[A](init: A)(f: (A, O) => A)(implicit F: MonadThrow[F]):  F[A] = F.map(self.fold(init)(f))(_._2)

      def toList(implicit F: MonadThrow[F]): F[List[O]] = self.toList

      def take(n: Int): Stream[F, O] =  resultMonad.void(self.take(n)) // self.take(n).map(_ => ())

      def ++(that: => Stream[F, O]): Stream[F, O] = self >> that

      def repeat: Stream[F, O] = self.repeat

      def filter(p: O => Boolean): Stream[F, O] = self.filter(p)

      def map[O2](f: O => O2): Stream[F, O2] = self.mapOutput(f)

      def flatMap[O2](f: O => Stream[F, O2]): Stream[F, O2] = flatMapOutput(self)(f)

      def mapEval[O2](f: O => F[O2]): Stream[F, O2] = flatMap(o => eval(f(o)))

      def handleErrorWith(handler: Throwable => Stream[F, O]): Stream[F, O] = Handle(self, handler)

      /**
       * allow us to evaluate a stream after the completion of a source stream. regardless of whether the source stream
       * completed successfully or failed with an error
       * */
      def onComplete(that: => Stream[F, O]): Stream[F, O] = handleErrorWith(t => that ++ raiseError(t)) ++ that
    }

    def run[F[_], O](s: Stream[F, O])(implicit M: MonadThrow[F]): F[Unit] = M.map(s.fold(())((_, _) => ()))(_._1)

    implicit class StreamNothingOps[O](self: Stream[Nothing, O]) {
      def fold[A](init: A)(f: (A, O) => A): A = ??? // self.fold(init)(f)(Monad.tailrecMonad).result._2

      def toList: List[O] = ??? // self.toList(Monad.tailrecMonad).result
    }

    def unit[F[_], A](a: => A): Stream[F, A] = Output(a)
    def flatMap[F[_], A, B](a: Stream[F, A])(f: A => Stream[F, B]): Stream[F, B] = flatMapOutput(a)(f)
  }

  // A more expressive Pipe definition:
  // type Pipe[-F[_], -I, +G[_], +O] = Stream[F, I] => Stream[G, O]
  import Stream._
  type Pipe[F[_], -I, +O] = Stream[F, I] => Stream[F, O]

  // Example definition of sinks which is just Pipe which change output to [[Unit]] or [[Nothing]]

  /** log sink println each stream element to console */
  def log[O]: Pipe[IO, O, Unit] = _.mapEval(o => IO(println(o)))

  /** drain sink discard all element of source */
  def drain[F[_], O]: Pipe[F, O, Nothing] = src => Stream.flatMap(src)(_ => Stream.empty)

}

object EffectfulPullsExample {
  import ErrorHandlingPull.Stream
  import ErrorHandlingPull.Stream.StreamOps
  def acquire(path: String): Task[Source] = Task(IO { Try (Source.fromFile(path)) })

  def use(source: Source): Stream[Task, Unit] = {
    Pull.flatMapOutput(Stream.eval(Task(IO { Try(source.getLines()) })))(itr => Stream.fromIterator(itr))
      .mapEval(line => Task(IO { Try(println(line))}))
  }

  def release(source: Source): Task[Unit] = Task(IO { Try (source.close()) })

  val printLines: Stream[Task, Unit] = {
    Stream.flatMap(
      Stream.eval(acquire("test/path/to/file")) // evaluate acquire Task to open file
    )(resource =>
      use(resource) //print each line to the console
        .onComplete(Stream.eval(release(resource))) // release the opened file using onComplete
    )
  }

  /*
  * a major limitation with this approach: Not composable
  * if final mapEval fail then the error registered to onComplete will not be called
  * and resource will never be finalized
  */
  printLines.mapEval(line => Task(IO { Try(println(line)) })) // not finalized on error in mapEval
  printLines.take(10) // not finalized because take is performed after onComplete

  /**
   * [[Stream.flatMap]] and [[Stream.StreamOps.onComplete]] alone does not guarantee the finalization of stream
   * Add ErrorHandling to stream result in stronger constraint on its elimination form. it's no longer possible
   * to convert between `Stream[IO, X]` into `IO[List[X]]` or any other `IO` value as they don't have a [[MonadThrow]].
   * Most Monad though have support for error handling
   *
   * for non-effectual stream, we previously evaluated them using TailRec Monad which doesn't have a [[MonadThrow]] but
   * can be easily added
   * `type SyncTask = TailRec[Try[A]]`
   * `MonadThrow[SyncTask]`
   * */
}

