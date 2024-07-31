package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter10Monoid.Monoid
import exercieAnswers.chapter13IO.{Monad, Task}
import exercieAnswers.chapter13IO.IOApp.IO

import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success}

object Resources {
  private trait Delay[F[_]] {
    def wait[A](a: () => A): F[A]
  }

  /**
   * purely functional equivalent of a variable of type A
   * where all mutation is performed in the effect F
   * */
  final class Ref[F[_], A] private (
    underlying: AtomicReference[A],
    delay: Delay[F]
  ) {
    def get: F[A] = delay.wait(() => underlying.get())
    def set(a: A): F[Unit] = delay.wait(() => underlying.set(a))
    def modify[B](f: A => (A, B)): F[B] = delay.wait { () =>
      @tailrec
      def loop(): B = {
        val oldA = underlying.get()
        val (newA, result) = f(oldA)
        if (underlying.compareAndSet(oldA, newA)) result else loop()
      }
      loop()
    }
  }

  object Ref {
    def apply[F[_], B](initial: B)(implicit F: Monad[F]): Ref[F, B] = new Ref[F, B](
      new AtomicReference[B](initial),
      new Delay[F] {
        def wait[C](a: () => C): F[C] = F.map(F.unit(()))(_ => a())
      }
    )
  }

  final class Id

  /** Using [[Ref]] to implement [[Scope]] as a state machine with [[Open]] and [[Closed]] states */
  final class Scope[F[_]](parent: Option[Scope[F]], val id: Id, state: Ref[F, State[F]])(implicit F: MonadThrow[F]) {
    def open(finalizer: F[Unit]): F[Scope[F]] = {
      val nested = state.modify {
        // construct new scope for new finalizer
        // compute new Open State which references the new scope in it subscopes
      case Open(myFinalizer, subscopes) =>
        val sub = new Scope(Some(this), new Id, Ref(Open(finalizer, Vector.empty)))
        (Open(myFinalizer, subscopes :+ sub), F.unit(sub))
        //
      case Closed() =>
        // propagate the open request to parent scope
        // parent scope has no Parent and throw error if propagation reach parent and still closed
        val next = parent match {
          case None => F.raiseError[Scope[F]](new RuntimeException("root scope already closed"))
          case Some(p) => p.open(finalizer)
        }
        (Closed(), next)
      }
      F.flatMap(nested)(identity)
    }

    def close: F[Unit] = state.modify {
      case Open(finalizer, subscopes) =>
        // reverse the finalizer to run them in stack like fashion and keep running even if error happen
        val finalizers = (subscopes.reverseIterator.map(_.close) ++ Iterator(finalizer)).toList
        def go(rem: List[F[Unit]], error: Option[Throwable]): F[Unit] = {
          rem match {
            case Nil => error match {
              case None => F.unit(())
              case Some(t) => F.raiseError(t)
            }
            case hd :: tl =>
              import F.MonadThrowOps
              //
              F.flatMap(hd.attempt)(res => go(tl, error orElse res.toEither.swap.toOption))
          }
        }
        // Change the open scope state to close
        (Closed(), go(finalizers, None))
      case Closed() => (Closed(), F.unit(()))
    }


    def findScope(target: Id): F[Option[Scope[F]]] = ???
  }


  sealed trait State[F[_]]
  case class Open[F[_]](finalizer: F[Unit], subscopes: Vector[Scope[F]]) extends State[F]
  case class Closed[F[_]]() extends State[F]

  object Scope {
    def root[F[_]](implicit M: MonadThrow[F]): Scope[F] = new Scope(
      None,
      new Id,
      Ref(Open(M.unit(()), Vector.empty))
    )
  }

  import Pull._
  trait Pull[+F[_], +O, +R] {
    def flatMap[F2[x] >: F[x], O2 >: O, R2](f: R => Pull[F2, O2, R2]): Pull[F2, O2, R2] = FlatMap(this, f)
    def step[F2[x] >: F[x], O2 >: O, R2 >: R](scope: Scope[F2])(implicit F: MonadThrow[F2]): F2[StepResult[F2, O2, R2]] = this match {
      case Result(r) => F.unit(Done(scope, r)) // use Monad instance for Effect type to lift result into effect type
      case Output(o: O) => F.unit(Out(scope, o, Pull.done)) // use Monad instance for Effect type to lift result into effect type
      case Eval(action) => F.map(action)(r => Done(scope, r)) // map result of action in a Left
      case Uncons(source) => F.map(source.step(scope)(F))(s => Done(scope, s.asInstanceOf[R2]))
      case Handle(source, f) => source match {
        case Handle(s2, g) => s2.handleErrorWith(x => g(x).handleErrorWith(y => f(y))).step(scope) // Rewrite left nested handler to right nested handler
        case other =>
          import F.MonadThrowOps
          F.map(other.step(scope)) {
            case Out(scope, hd, tl) => Out(scope, hd, Handle(tl, f))
            case Done(scope, r) => Done(scope, r)
          }.handleErrorWith(t => f(t).step(scope)) // handle error occur when stepping source pull
      }
      case FlatMap(source, f) => source match {
        case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(f)).step(scope)
        case other => F.flatMap(other.step(scope)(F)) { // recursively step using flatMap to access result of first step
          case Done(scope, r) => f(r).step(scope)
          case Out(scope, hd, tl) => F.unit(Out(scope, hd, tl.flatMap(f)))
        }
      }
      case OpenScope(source, finalizer) =>
        // delegate to open on active scope
        F.flatMap(scope.open(finalizer.getOrElse(F.unit(())))) {
          // evaluate the original source stream using newly opened subscope
          subscope => WithScope(source, subscope.id, scope.id).step(subscope)
        }

      case WithScope(source, scopeId, returnScopeId) =>
        import F.MonadThrowOps
        // first search scope tree for target scope to use for stepping source stream
        F.flatMap(F.map(scope.findScope(scopeId))(_.map(_ -> true).getOrElse(scope -> false))) {
          case (newScope, _closeAfterUse) => F.flatMap(source.step(newScope).attempt) {
            // propagate WithScope through the remaining stream when Out
            case Success(Out(scope, hd, tl)) => F.unit(Out(scope, hd, WithScope(tl, scopeId, returnScopeId)))
            // find scope to return to
            case Success(Done(outScope, r)) => F.flatMap(F.map(scope.findScope(returnScopeId))(_.getOrElse(outScope))) {
              // then close target scope
              nextScope => F.as(scope.close)(Done(nextScope, r))
            }
            case Failure(t) => F.flatMap(scope.close)(_ => F.raiseError(t))
          }
        }

    }

    def handleErrorWith[F2[x] >: F[x], O2 >: O, R2 >: R](handler: Throwable => Pull[F2, O2, R2]): Handle[F2, O2, R2] = Handle(this, handler)

    /** similar to [[step]], fold is no longer tail recursive and stack-safety is depend on Effect Type Monad */
    def fold[F2[x] >: F[x], R2 >: R, A](init: A)(f: (A, O) => A) (implicit F: MonadThrow[F2]): F2[(R2, A)] = {
      import F.MonadThrowOps
      val scope = Scope.root[F2]
      def go(scope: Scope[F2], p: Pull[F2, O, R2], acc: A): F2[(R2, A)] = {
        F.flatMap(step(scope)(F))
        {
          case Done(_, r) => F.unit((r, init))
          case Out(newScope, hd, tl) => go(newScope, tl, f(init, hd))
        }
      }

      F.flatMap(go(scope, this, init).attempt) { res =>
        F.flatMap(scope.close) { _ =>
          res.fold(F.raiseError, F.unit)
        }
      }
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
  // upon encountering an open scope, the interpreter calls open on the active scope and pass the finalizer
  case class OpenScope[+F[_], +O, +R](source: Pull[F, O, R], finalizer: Option[F[Unit]]) extends Pull[F, O, R]

  // source pull and scopeId to be use during source pull interpretation. returnScopeId is return after source pull terminate
  case class WithScope[+F[_], +O, +R](source: Pull[F, O, R], scopeId: Id, returnScopeId: Id) extends Pull[F, O, R]

  trait StepResult[F[_], +O, +R]
  case class Done[F[_], O, R](scope: Scope[F], result: R) extends StepResult[F, O, R]
  case class Out[F[_], O, R](scope: Scope[F], head: O, tail: Pull[F, O, R]) extends StepResult[F, O, R]


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
}

