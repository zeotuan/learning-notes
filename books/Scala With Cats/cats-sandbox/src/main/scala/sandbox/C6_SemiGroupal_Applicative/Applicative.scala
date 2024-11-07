package sandbox.C6_SemiGroupal_Applicative

import cats.Functor

/**
 * Applicative is related to Apply as Monoid is related to SemiGroup
 *
 * every Monad is an Applicative
 * every Applicative is an Apply
 * every Apply is a Semigroupal and Functor
 *
 * SemiGroupal - product    Functor - map
 *                      \  /
 *                      Apply - ap
 *                      /  \
 *    Applicative - pure    FlatMap - flatMap
 *                      \  /
 *                      Monad
 *
 * Apply define product in term of ap and map
 * Moand define product in term of flatMap and pure
 *
 *
 * Applicative and Monad represent trade off between flexibility and power
 * Applicative is more flexible but we don't get the strong guarantees of behavior
 * Monad is more powerful and we get the strong guarantees of strict sequencing on the computation but lose flexibility
 * */

trait RawApply[F[_]] extends Semigroupal[F] with Functor[F] {
  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    val fab: F[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
    ap(fab)(fb)
  }
}

trait RawApplicative[F[_]] extends RawApply[F] {
  def pure[A](a: A): F[A]
}


