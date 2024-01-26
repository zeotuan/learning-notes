package exercieAnswers.chapter12Applicative

import exercieAnswers.chapter06State.State
import exercieAnswers.chapter10Monoid.{Foldable, Monoid}
import exercieAnswers.chapter11Monad.Functor
import exercieAnswers.chapter11Monad.Monad.stateMonad
import exercieAnswers.chapter12Applicative.Applicative.{Const, monoidApplicative}

import scala.annotation.tailrec

/**
 * Traversable functos let us compose applicative effect with various iterations patterns, avoiding
 * the need to write specialized logic for each effect
 *
 * a traversable instance is defined by providing implementations of either `traverse` or both `map` and `sequence`
 *
 * A traversal is similar to a fold in that both take some data structure and apply a function to the data within to produce a result.
 * The difference is that traverse preserves the original structure, whereas foldMap discards the structure and replaces it with the
 * operations of a monoid. Look at the signature `Tree[Option[A]] => Option[Tree[A]]`, for instance. We’re preserving the Tree structure,
 * not merely collapsing the values using some monoid.
 *
 * Foldable itself cannot extend Functor
 * */
trait Traverse[F[_]] extends Functor[F] with Foldable[F]{
 /** takes an F[A] and `A => G[B]` and returns a `G[F[B]]`, as long as there’s a Traverse[F] instance and an Applicative[G] instance */
 def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))

 /** swapping order of F and G to convert an `F[G[A]]` to `G[F[A]]` as long as there is a Traverse[F] instance and an Applicative[G] instance  */
 def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)

 type Id[A] = A

 val idMonad = new Monad[Id] {
  override def unit[A](a: => A): Id[A] = a
  override def flatMap[A, B](fa: A)(f: A => Id[B]): Id[B] = f(fa)
 }
 def map[A, B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(f)(idMonad)

 override def foldMap[A, M](as: F[A])(f: A => M)(mb: Monoid[M]): M = traverse[({type f[x] = Const[M, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

 /**  */
 def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(stateMonad)


 /** State traversal that labels every elements with its position*/
  def zipWithIndex_[A](fa: F[A]): F[(A,Int)] = traverseS(fa)(a => for {
    i <- State.get[Int]
    _ <- State.set(i + 1)
   } yield (a, i)
  ).run(0)._1

 /** Turn any Traversable functor to List */
 def toList_[A](fa: F[A]): List[A] = traverseS(fa)(a => for {
   as <- State.get[List[A]] // Get the current state, the accumulated list.
   _  <- State.set(a :: as) // Add the current element and set the new list as the new state.
  // yield () since we don't want to return any value rather than state
  // start with Nil
  } yield ()).run(Nil)._2.reverse

 def mapAccum[A, S, B](fa: F[A])(s: S)(f: (A, S) => (B, S)): (F[B], S) = traverseS(fa)(a => for {
  s1 <- State.get[S]
  (b, s2) = f(a, s1)
  _ <- State.set(s2)
 } yield b).run(s)

 override def toList[A](fa: F[A]): List[A] = mapAccum(fa)(List.empty[A])((a, s) => ((), a :: s))._2.reverse
 def zipWithIndex[A](fa: F[A]): F[(A, Int)] = mapAccum(fa)(0)((a, s) => ((a, s), s + 1))._1

 /**
  * Exercise 12.16: Implement reverse
  * with mapAccum, each element in F[A] is replaced with a transformed element of B - producing an F[B]
  * but unlikes map, the computation rely on accumulated state
  * reverse can be think of as simply replacing an element with it's corresponding element at it's reversal in same position
  *
  * to perform this, we can first get reversed list version of F[A]
  * use this reversed list of initial state and for each element, we replace it's with the head of this initial list
  * after each iteration, remove the head from the state
  * */
 def reverse[A](fa: F[A]): F[A] = mapAccum(fa)(toList(fa).reverse)((_, as) => (as.head, as.tail))._1

 /**
  * Exercise 12.17: Implement foldLeft in term of mapAccum
  * mapAccum accumulate state likes foldLeft not only that, it also outputs each transformed element
  * so we can simply use mapAccum likes foldLeft, except we don't care about transformed element only the state
  * so we can pass () for each transformed element which make mapAccum return (F[Unit], B)
  * we then get B and simply discard F[Unit]
  * */
 override def foldLeft[A, B](as: F[A])(acc: B)(f: (B, A) => B): B = mapAccum(as)(acc)((a, c) => ((), f(c, a)))._2

 /**
  * this zip cannot handle arguments of different shapes.
  * for example:
  * - list fa must has the same length as list fb
  * - tree fa must have same level of branches as fb at every level
  * */
 def zip[A, B](fa: F[A])(fb: F[B]): F[(A, B)] = mapAccum(fa)(toList(fb)) {
  case (a, Nil) => sys.error("cannot zip incompatible shape")
  case (a, b :: bs) => ((a, b), bs)
 }._1

 def zipL[A, B](fa: F[A])(fb: F[B]): F[(A, Option[B])] = mapAccum(fa)(toList(fb)) {
  case (a, Nil) => ((a, None), Nil)
  case (a, b :: bs) => ((a, Some(b): Option[B]), bs)
 }._1

 def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = mapAccum(fb)(toList(fa)) {
  case (b, Nil) => ((None, b), Nil)
  case (b, a :: as) => ((Some(a), b), as)
 }._1

 /**
  * Exercise 12.18: use applicative functor to write fusion of two traversals
  * given two function f and g, traverse fa a single times and collect result of both at once
  * */
 def fuse[M[_]: Applicative,N[_]: Applicative,A,B](fa: F[A])(f: A => M[B], g: A => N[B])(implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
  traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)

 def compose[G[_]: Traverse](implicit g: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = {
  val self = this
  new Traverse[({ type f[x] = F[G[x]] })#f] {
   override def traverse[M[_]: Applicative, A, B](fga: F[G[A]])(f: A => M[B]): M[F[G[B]]] = self.traverse(fga)(ga => g.traverse(ga)(f))
  }
 }
}

case class Tree[+A](head: A, tail: List[Tree[A]])
object Traverse {
 /**
  * List[Option[A]] => Option[List[A]] (a call to Traverse[List].sequence, with Option as the Applicative) returns None
  * if any of the input List is None; otherwise, it returns a Some containing a List of all the values in the input List.
  */
 val listTraverse = new Traverse[List] {
  def traverse[G[_] : Applicative, A, B](fa: List[A])(f: A => G[B])(implicit g: Applicative[G]): G[List[B]] =
   fa.foldRight(g.unit(List.empty[B]))((a, fbs) => g.map2(f(a))(fbs)(_ :: _))
  override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
 }

 val optionTraverse = new Traverse[Option] {
  def traverse[G[_] : Applicative, A, B](fa: Option[A])(f: A => G[B])(implicit g: Applicative[G]): G[Option[B]] = fa match {
   case Some(a) => g.map(f(a))(Some(_))
   case None => g.unit(None)
  }
  override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
 }

 /**
  * Tree[Option[A]] => Option[Tree[A]] (a call to Traverse[Tree].sequence, with Option as the Applicative) returns None
  * if any of the input Tree is None; otherwise, it returns a Some containing a Tree of all the values in the input Tree.
  * */
 val treeTraverse = new Traverse[Tree] {
  override def traverse[G[_], A, B](ta: Tree[A])(f: A => G[B])(implicit g: Applicative[G]): G[Tree[B]] =
   g.map2(f(ta.head))(listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = Tree(f(fa.head), fa.tail.map(map(_)(f)))
 }

 /**
  * Map[K, Par[A]] => Par[Map[K, A]] (a call to Traverse[Map[K, _]].sequence with Par as the Applicative) produces a
  * parallel computation that evaluates all values of the map in parallel.
  * */
 def mapTraverse[K]: Traverse[({type f[x] = Map[K, x]})#f] = new Traverse[({type f[x] = Map[K, x]})#f] {
  def traverse[G[_] : Applicative,A, B](ma: Map[K, A])(fa: Map[K, A])(f: A => G[B])(implicit g: Applicative[G]): G[Map[K, B]] = ma.foldLeft(g.unit(Map.empty[K, B])) {
   case (acc, (k, a)) => g.map2(acc)(f(a))((m, b) => m + (k -> b))
  }
  override def map[A, B](ma: Map[K, A])(f: A => B): Map[K, B] = ma.map {case (k, a) => (k, f(a))}
 }


 /** Showing that not all foldable is a functor */
 case class Iteration[A](a: A, f: A => A, n: Int) extends Foldable[Iteration] with Functor[Iteration] {
  override def foldMap[A, B](as: Iteration[A])(f: A => B)(mb: Monoid[B]): B = {
   @tailrec
   def iterate(n: Int, b: B, c: A): B = if (n <= 0)
    b
   else
    iterate(n - 1, f(c), as.f(as.a))
   iterate(as.n, mb.empty, as.a)
  }

  /**
   * There is no way of putting these functions together to form a b => b
   * Foldable let us visit each element. Foldable does not us construct new value of foldable type
   * while Functor let us change elements while preserving overall structure.
   * */
  override def map[A, B](fa: Iteration[A])(f: A => B): Iteration[B] = Iteration(f(fa.a),b => ???, fa.n)
 }
}
