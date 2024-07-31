package exercieAnswers.chapter13IO

import language.higherKinds
import language.implicitConversions
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

trait Functor[F[_]] {
  def map[A,B](a: F[A])(f: A => B): F[B]
}

/**
 * This is specific Monad implementation for IO which
 * might contain function that does not make sense for Option
 * or LazyList Monad
 *
 * Although this imperative style implementation is not the recommended way in Scala.
 * This prove FP and scala expressiveness power and that any program can be expressed
 * in purely functional style.
 *
 * */
trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](a: F[A])(f: A => F[B]): F[B]

  def map[A,B](a: F[A])(f: A => B): F[B] = flatMap(a)(a => unit(f(a)))
  def map2[A,B,C](a: F[A], b: F[B])(f: (A,B) => C): F[C] =
    flatMap(a)(a => map(b)(b => f(a,b)))
  def sequence_[A](fs: LazyList[F[A]]): F[Unit] = foreachM(fs)(void)
  def sequence_[A](fs: F[A]*): F[Unit] = sequence_(fs.to(LazyList))
  def replicateM[A](n: Int)(f: F[A]): F[List[A]] =
    LazyList.fill(n)(f).foldRight(unit(List[A]()))(map2(_,_)(_ :: _))
  def replicateM_[A](n: Int)(f: F[A]): F[Unit] =
    foreachM(LazyList.fill(n)(f))(void)
  def as[A,B](a: F[A])(b: B): F[B] = map(a)(_ => b)

  /** Replace value with Unit */
  def void[A](a: F[A]): F[Unit] = as(a)(())
  def when[A](b: Boolean)(fa: => F[A]): F[Boolean] =
    if (b) as(fa)(true) else unit(false)

  /** repeat the effect of monad indefinitely */
  def forever[A,B](a: F[A]): F[B] = {
    lazy val t: F[B] = a flatMap (_ => t)
    t
  }
  def while_(a: F[Boolean])(b: F[Unit]): F[Unit] = {
    lazy val t: F[Unit] = while_(a)(b)
    a flatMap (c => void(when(c)(t)))
  }

  /** Repeat effect of monad as long as condition yields true */
  def doWhile[A](a: F[A])(cond: A => F[Boolean]): F[Unit] = for {
    a1 <- a
    ok <- cond(a1)
    _ <- if (ok) doWhile(a)(cond) else unit(())
  } yield ()

  /** Folds the Stream with the function f, combining the effects and return-ing the result */
  def foldM[A,B](l: LazyList[A])(z: B)(f: (B,A) => F[B]): F[B] =
    l match {
      case h #:: t => f(z,h) flatMap (z2 => foldM(t)(z2)(f))
      case _ => unit(z)
    }

  /** same as foldM but ignore the final result */
  def foldM_[A,B](l: LazyList[A])(z: B)(f: (B,A) => F[B]): F[Unit] =
    void { foldM(l)(z)(f) }

  /** Calls the function f for each element of the stream and combines the effects */
  def foreachM[A](l: LazyList[A])(f: A => F[Unit]): F[Unit] =
    foldM_(l)(())((u,a) => void(f(a)))
  def seq[A,B,C](f: A => F[B])(g: B => F[C]): A => F[C] =
    f andThen (fb => flatMap(fb)(g))

  // syntax
  implicit def toMonadic[A](a: F[A]): Monadic[F,A] =
    new Monadic[F,A] { val F = Monad.this; def get = a }

  implicit class NestedMonadOps[A](ffa: F[F[A]]) {
    def flatten: F[A] = ffa.flatMap(identity)
  }
}

trait Monadic[F[_],A] {
  val F: Monad[F]
  import F._
  def get: F[A]
  private val a = get
  def map[B](f: A => B): F[B] = F.map(a)(f)
  def flatMap[B](f: A => F[B]): F[B] = F.flatMap(a)(f)
  def **[B](b: F[B]) = F.map2(a,b)((_,_))
  def *>[B](b: F[B]) = F.map2(a,b)((_,b) => b)
  def map2[B,C](b: F[B])(f: (A,B) => C): F[C] = F.map2(a,b)(f)
  def as[B](b: B): F[B] = F.as(a)(b)
  def void: F[Unit] = F.void(a)
  def replicateM(n: Int) = F.replicateM(n)(a)
  def replicateM_(n: Int) = F.replicateM_(n)(a)
}

object Monad {
  implicit def tailrecMonad: Monad[TailRec] = new Monad[TailRec] {
    def unit[A](a: => A): TailRec[A] = TailCalls.done(a)
    def flatMap[A, B](a: TailRec[A])(f: A => TailRec[B]): TailRec[B] = a.flatMap(f)
  }
}
