package exercieAnswers

import scala.annotation.tailrec
import collection.mutable.ListBuffer

// Manual List implementation
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](elems: A*): List[A] = if (elems.isEmpty)
    Nil
  else
    Cons(elems.head, apply(elems.tail: _*))

  // Exercise 3.2: implement function tail which remove the head of a list
  def tail[A](elems: List[A]): List[A] = elems match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3: implement function setHead which replace the head of a list
  def setHead[A](elems: List[A], h: A): List[A] = elems match {
    case Nil => throw new Exception("Cannot set head of empty List")
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4: drop first n elements from a list
  def drop[A](elems: List[A], n: Int): List[A] = elems match {
    case Cons(_, tail) if n > 0 => drop(tail, n-1)
    case _ => elems
  }

  // Exercise 3.5: drop list elements as long as predicate is satisfied
  def dropWhile[A](elems: List[A], f: A => Boolean): List[A] = elems match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => elems
  }

  def append[A](elems1: List[A], elems2: List[A]): List[A] = elems1 match {
    case Nil => elems2
    case Cons(head, tail) => Cons(head, append(tail, elems2))
  }

  // Exercise 3.6: list init function
  // inefficient:
  // - non tail recursive => potential stackoverflow
  // - excessive copy of tail elements until the last element
  def init[A](elems: List[A]): List[A] = elems match {
    case Nil => throw new Exception("Cannot remove element of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  // a common solution being used is to make use of mutable data structure such as stream, lazy lists
  // even though we try to avoid using var and mutable data structure as much as possible
  // as long as the buffer is allocated internally in the function scope, RT is still preserved
  def innitMutable[A](elems: List[A]): List[A] = {
    val internalBuffer = new ListBuffer[A]()

    @tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => throw new Exception("Cannot remove element of empty list")
      case Cons(_, Nil) => List(internalBuffer.toSeq: _*)
      case Cons(h, t) =>
        internalBuffer += h
        go(t)
    }

    go(elems)
  }


  // Recursion over list and generalisation to higher order function

  // Polymorphic sum on List[Int]
  def sum(elems: List[Int]): Int = elems match {
    case Nil => 0
    case Cons(head, tail) => head + sum(tail)
  }

  // Polymorphic product on List[Double]
  def product(elems: List[Double]) : Double = elems match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(head, tail) => head * product(tail)
  }

  // generic monomorphic foldRight function
  // Exercise 3.7: Because foldRight apply accumulate and apply f in reverse order
  // Therefore function is never really called until we reach the end of the list
  // Making short circuiting using foldRight impossible
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = as match {
    case Nil => acc
    case Cons(h, t) => f(h, foldRight(t, acc, f))
  }

  // Exercise 3.8: Passing Nil function return into fold right get us our original list back
  //  foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil: List[Int])(Cons(_, _))
  //  Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_, _)))
  //  Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil: List[Int])(Cons(_, _))))
  //  Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil: List[Int])(Cons(_, _)))))
  //  Cons(1, Cons(2, Cons(3, Nil)))
  def sumViaFoldRight(elems: List[Int]): Int = foldRight(elems, 0, (x: Int, y: Int) => x + y)
  def productViaFoldRight(elems: List[Int]): Int = foldRight(elems, 0, (x: Int, y: Int) => x * y)

  // Exercise 3.9: lengthViaFoldRight
  def lengthViaFoldRight[A](elems: List[A]): Int = foldRight(elems, 0, (_: A, y: Int) => y + 1)

  // Exercise 3.10: implement tail recursive foldLeft
  // P.s. the function signature here is different from the book to align with foldRight
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f:(A, B) => B): B = as match {
    case Nil => acc
    case Cons(h, t) => foldLeft(t, f(h, acc), f)
  }

  // Exercise 3.11: sum, product, length via foldLeft
  def sumViaFoldLeft(as: List[Int]): Int = foldLeft(as, 0, (x: Int, y: Int) => x + y)
  def productViaFoldLeft(as: List[Double]): Double = foldLeft(as, 1, (x: Double, y: Double) => x * y)
  def lengthViaFoldLeft[A](as: List[A]): Int = foldLeft(as, 0, (_: A, y: Int) => y + 1)

  // Exercise 3.12: reverse using fold
  // the most efficient implementation is to use foldLeft
  // since foldLeft go through and apply f left to right
  // Therefore, we create a Cons where current element is head and accumulate element are tail
  def reverseViaFoldLeft[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A], (x: A, y: List[A]) => Cons(x, y))

  // Less efficient implementation using foldRight
  def reverseViaFoldRight[A](as: List[A]): List[A] = foldRight(as, Nil: List[A], (x: A, y: List[A]) => append(y, List(x)))

  // Exercise 3.13: Implement foldRight using foldLeft
  def foldRightViaFoldleftUsingReverse[A, B](as: List[A], acc: B, f:(A, B) => B): B = foldLeft(reverseViaFoldLeft(as), acc, f)

  // another cool implementation is to build up a chain of functions by accumulating identity function B => B
  // even though foldLeft is tail recursive, this is not stack-safe if list length excess stack limit
  def foldRightViaFoldLeft[A, B](as: List[A], acc: B, f:(A, B) => B): B = foldLeft[A, B => B](
    as,
    // Identity function B => B
    (b: B) => b,
    // (A, B => B) => (B => B)
    (a: A, g: B => B) => b => g(f(a,b))
  )(acc)

  // implement foldLeft purely via foldRight would be very similar
  def foldLeftViaFoldRightUsingReverse[A, B](as: List[A], acc: B, f: (A, B) => B): B = foldRight[A, B](reverseViaFoldRight(as), acc, f)
  def foldLeftViaFoldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = foldRight[A, B => B](
    as,
    (b :B) => b,
    (a: A, g: B => B) => b => g(f(a, b))
  )(acc)



  // Exercise 3.14: Implement concat using foldLeft
  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] = foldRight(l, r, Cons(_,_))

  // Exercise 3.15: Implement concat
  def concatViaFoldRight[A](ls: List[List[A]]) = foldRight[List[A], List[A]](ls, Nil: List[A], appendViaFoldRight)

  // Exercise 3.16:  Implement a function that increment each element by 1
  def incrementEach(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int], (h: Int, t: List[Int]) => Cons(h + 1, t))

  // Exercise 3.17:  Implement a function that convert each element to string
  def toStringEach[A](as: List[A]): List[String] = foldRight(as, Nil: List[String], (h: A, t: List[String]) => Cons(h.toString, t))



  // Exercise 3.18: Implement a polymorphic map function
  // variation 0: foldRight are used for simplicity as it kept the result list in correct order
  def map[A, B](as: List[A], f: A => B): List[B] = foldRight(as, Nil: List[B], (h: A, t: List[B]) => Cons(f(h), t))

  // variation 1: use foldRightViaFoldLeftUsingReverse instead to be stack-safe
  def mapVar1[A, B](as: List[A], f: A => B): List[B] = foldRightViaFoldLeft(as, Nil: List[B], (h: A, t: List[B]) => Cons(f(h), t))

  // variation 3: use local scope mutation data structure to optimize our implementation which preserve RT as  hide away mutation
  // Implementation should be very similar to initMutable
  def mapVar2[A, B](as: List[A], f: A => B): List[B] = {
    val internalBuffer = new ListBuffer[B]()

    @tailrec
    def go(l:  List[A]): List[B] = l match {
      case Cons(h, t) =>
        internalBuffer += f(h)
        go(t)
      case _ => List(internalBuffer.toSeq: _*)
    }

    go(as)
  }

  // Exercise 3.19: Implement list filter
  def filter[A](as: List[A], f: A => Boolean): List[A] = foldRight(
    as,
    Nil: List[A],
    (h: A, t: List[A]) => if (f(h)) t else Cons(h, t)
  )

}