package exercieAnswers.chapter10Monoid

import exercieAnswers.chapter03DataStructures.{Tree, Leaf, Branch}


/**
 * In this Foldable trait we implemented foldMap using foldLeft
 * foldRight and foldLeft is then implemented in term of foldMap
 * This leave no abstract implementation left
 * However, when extending Foldable,
 * we should reimplement either foldLeft or foldMap to avoid infinite loop
 *  */
trait Foldable[F[_]] {
  import Monoid._
  def foldRight[A, B](as: F[A])(acc: B)(f: (A, B) => B): B = foldMap(as)(f.curried)(dual(endoMonoid[B]))(acc)
  def foldLeft[A, B](as: F[A])(acc: B)(f: (B, A) => B): B = foldMap(as)(a => (b: B) => f(b, a))(endoMonoid[B])(acc)
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = foldLeft(as)(mb.empty)((b, a) => mb.combine(b, f(a)))
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.empty)(m.combine)
  def toList[A](as: F[A]): List[A] = foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B = as.foldLeft(acc)(f)
  override def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B  = as.foldRight(acc)(f)
  override def toList[A](as: List[A]): List[A] = as
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid.foldMapV
  override def foldLeft[A, B](as: IndexedSeq[A])(acc: B)(f: (B, A) => B): B = as.foldLeft(acc)(f)
  override def foldRight[A, B](as: IndexedSeq[A])(acc: B)(f: (A, B) => B): B = as.foldRight(acc)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
}

object LazyListFoldable extends Foldable[LazyList] {
  override def foldLeft[A, B](as: LazyList[A])(acc: B)(f: (B, A) => B): B = as.foldLeft(acc)(f)
  override def foldRight[A, B](as: LazyList[A])(acc: B)(f: (A, B) => B): B = as.foldRight(acc)(f)
}

/** Exercise 10.13: */
object TreeFoldable extends Foldable[Tree] {
  override def foldLeft[A, B](as: Tree[A])(acc: B)(f: (B, A) => B): B = as match {
    case Leaf(v) => f(acc, v)
    // Fold left branch first then right branch
    case Branch(l, r) => foldLeft(r)(foldLeft(l)(acc)(f))(f)
  }
  override def foldRight[A, B](as: Tree[A])(acc: B)(f: (A, B) => B): B = as match {
    case Leaf(v) => f(v, acc)
    // Fold right branch first then left branch
    case Branch(l, r) => foldRight(l)(foldRight(r)(acc)(f))(f)
  }
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => mb.combine(foldMap(l)(f), foldMap(r)(f))
  }
}

/** Exercise 10.14 */
object OptionFoldable extends Foldable[Option] {

  override def foldLeft[A, B](as: Option[A])(acc: B)(f: (B, A) => B): B = as.map(a => f(acc, a)).getOrElse(acc)

  override def foldRight[A, B](as: Option[A])(acc: B)(f: (A, B) => B): B = as.map(a => f(a, acc)).getOrElse(acc)

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).getOrElse(mb.empty)
}
