package exercieAnswers.chapter05Laziness

import exercieAnswers.chapter05Laziness.LazyList.{cons, empty, fib, unfold}

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case object Empty extends LazyList[Nothing]
case class Cons[+A](head: () => A, tail: () => LazyList[A]) extends LazyList[A]
sealed trait LazyList[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // Exercise 5.1: Implement toList that turn LazyList into List
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def toListStackSafe: List[A] = {
    @tailrec
    def go(l: LazyList[A], acc: List[A]): List[A] = l match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc.reverse
    }
    go(this, Nil)
  }

  // optimized using internal buffer
  def toListFast: List[A] = {
    val internalBuffer = new ListBuffer[A]
    @tailrec
    def go(l: LazyList[A]): List[A] = l match {
      case Cons(h, t) =>
        internalBuffer += h()
        go(t())
      case Empty => internalBuffer.toList
    }

    go(this)
  }

 // Exercise 5.2: Implement take and drop function
  // this is actually type safe due to the fact that the list is lazyly evaluated
  def take(n: Int): LazyList[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1=> cons(h(), empty)
    case Empty => Empty
  }

  // only final function can be tailrec
  @tailrec
  final def drop(n: Int): LazyList[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  // Implement takeWhile function that takes all starting element satisfy a given predicate
  def takeWhile(p: A => Boolean): LazyList[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Empty => Empty
  }

  def exist(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exist(p)
    case _ => false
  }

  // => B mean the function f take the second argument B by name and may not choose to evaluate it
  def foldRight[B](acc: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(acc)(f))
    case _ => acc
  }

  def existViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4: Implement forAll which check if all list elements satisfy a predicate
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a,b) => p(a) && b)

  // Exercise 5.5: Implement takeWhile using foldRight
  def takeWhileViaFoldRight(p: A => Boolean): LazyList[A] = foldRight(empty)((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5.6: Implement headOption
  def headOptionViaFoldRight: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))


  // Exercise 5.7: Implement map, filter, append via foldRight
  def map[B](f: A => B): LazyList[B] = foldRight(empty: LazyList[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): LazyList[A] = foldRight(empty)((a,b) => if (f(a)) cons(a, b) else b)
  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight(that)((a,b) => cons(a, b))
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight(empty: LazyList[B])((a,b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // Exercise 5.13: Implement map, take, takeWhile, zipAll via unfold
  def mapViaUnfold[B](f: A => B): LazyList[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): LazyList[A] = unfold((this, n)) {
    case (Cons(h, _), 1)  => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(f: A => Boolean): LazyList[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  // (a,b) can be written as a -> b to reduce parentheses
  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] = unfold((this, that)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((Some(h()) -> None, t() -> empty))
    case (Empty, Cons(h, t)) => Some((None -> Some(h()), empty -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()) -> Some(h2()), t1() -> t2()))
  }

  def zipWith[B, C](that: LazyList[B])(f: (A,B) => C): LazyList[C] = unfold((this, that)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), t1() -> t2()))
    case _ => None
  }

  def zipWithAll[B, C](that: LazyList[B])(f: (Option[A], Option[B]) => C): LazyList[C] = unfold((this, that)) {
    case (Empty, Empty) => None
    case (Cons(h, t), Empty) => Some((f(Some(h()), None), t() -> empty))
    case (Empty, Cons(h, t)) => Some((f(None, Some(h())), empty -> t()))
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), t1() -> t2()))
  }

  // python zip
  def zip[B](that: LazyList[B]): LazyList[(A,B)] = zipWith(that)((_,_))

  def zipAllViaZipWithAll[B](s2: LazyList[B]): LazyList[(Option[A], Option[B])] = zipWithAll(s2)(_ -> _)

  // Exercise 5.14: Implement  startWith
  def startsWith[A](prefix: LazyList[A]): Boolean = zipAll(prefix).takeWhile(_._1.isDefined).forAll(a => a._1 == a._2)

  // Exercise 5.15: Implement tails functions that return a LazyList of all prefixes of this list
  // ex. LazyList(1,2,3) => LazyList(LazyList(1,2,3), LazyList(2,3), LazyList(3), LazyList(empty))
  def tails: LazyList[LazyList[A]] = unfold(this) {
    case Empty => None
    case Cons(h, t) => Some((Cons(h, t), t()))
  }.append(LazyList(empty))

  // LazyList allow composing function from multiple other helper function with sacrificing performance
  def hasSubsequence[A](l: LazyList[A]): Boolean = tails.exist(_.startsWith(l))

  // Exercise 5.16: Implement scanRight - generalize version of tails
  // similar to foldRight but return LazyList of intermediate result
  // unfold Implementation cannot be done properly without changing the semantic of the scanRight function
  // foldRight which going from Right to Left of the list allow us to lazyly compute list element from the bottom
  // we accumulate a tuple of (B, LazyList[B])
  // Where B is the latest B
  // LazyList[B] is the historical value of B

  def scanRight[B](init: B)(f: (A, => B) => B): LazyList[B] = foldRight[(B, LazyList[B])](init -> LazyList(init))((a, acc) => {
    lazy val lazyAcc = acc
    val computedA = f(a, lazyAcc._1)
    computedA -> cons(computedA, lazyAcc._2)
  })._2

  // LazyList(1, 2, 3).scanRight(0)(_ + _).toList
}

object LazyList {
  // Smart constructor for LazyList
  // h and t are thunk (function without param) which are cached as lazy value to avoid repeated evaluation
  //
  def cons[A](h: => A, t: => LazyList[A]): LazyList[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if (as.isEmpty)
      empty
    else
      cons(as.head, apply(as.tail: _*))


  // a infinite list of ones
  val ones: LazyList[Int] = cons(1, ones)

  /** Exercise 5.8: Implement a generic infinite List:
   * We would immediately think of `cons(a, continually(a))`
   * but this would involve lots more function call.
   * the optimization below allow us to have only one object referencing itself
  */
  def continually[A](a: A): LazyList[A] = {
    lazy val infiniteList = cons(a, infiniteList)
    infiniteList
  }

  // Exercise 5.9: Lazy infinite list that generate from n to infinity by incrementing 1
  def from(n: Int): LazyList[Int] = cons(n, from(n+1))

  // Exercise 5.10: infinite fibonacci sequence
  def fib: LazyList[Int] = {
    def go(current: Int, next: Int): LazyList[Int] = cons(current, go(next, current + next))
    go(0, 1)
  }

  /**
   * Exercise 5.11: Implement unfold which get the next list element and next state
   * Option is used for early termination
   * unfold is also sometimes called corecursive function or guarded recursion
   * recursive function:
   * - consume data
   * - terminate by recursing on "smaller" and "smaller" input
   * corecursive function:
   * - produce data as long as it is productive (productivity are also called cotermination)
   * - does not always terminate
  */
  def unfold[A,S](state: S)(f: S => Option[(A,S)]): LazyList[A] = f(state) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  // We can also implement unfold using build in fold which apply f to Option if value is not None
  def unfoldViaFold[A,S](state: S)(f: S => Option[(A,S)]): LazyList[A] = f(state).fold(empty)(p => cons(p._1, unfoldViaFold[A,S](p._2)(f)))
  def unfoldViaFold_[A,S](state: S)(f: S => Option[(A,S)]): LazyList[A] = f(state).map(p => cons(p._1, unfoldViaFold_[A,S](p._2)(f))).getOrElse(empty)

  // Exercise 5.12: Implement fib, from, continually  and ones via unfold
  def fibsViaUnfold: LazyList[Int] = unfold((0, 1)) {
    case (curr, next) => Some((curr, (next, curr + next)))
  }

  def fromViaUnfold(n: Int): LazyList[Int] = unfold(n)(now => Some((now, now+1)))
  def continuallyViaUnfold[A](a: A): LazyList[A] = unfold(())(_ => Some(a, ()))
  def oneViaUnfold: LazyList[Int] = continuallyViaUnfold(1)
}


