package exercieAnswers.chapter12Applicative

/**
 * Expressivity  and power sometimes come at the price of compositionality and modularity
 * the problem of composing monad is often address with custom written version of each monad
 * that's specifically constructed for composition. These things are called Monad Transformer.
 *
 *
 * */
case class OptionT[M[_],A](value: M[Option[A]])(implicit M: Monad[M]) {
  def unit(a: => A): OptionT[M, A] = OptionT(M.unit(Some(a)))

  /**
   * maps over both F and Option and flatten structure like `F[Option[F[Option[A]]]]` to just `F[Option[A]]`
   * This is specifically for option. However, he general strategy is to use Traverse only work if it's also a
   * Traversable Functor
   * */
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
    OptionT(M.flatMap(value) {
      case None => M.unit(None)
      case Some(a) => f(a).value
    })
}