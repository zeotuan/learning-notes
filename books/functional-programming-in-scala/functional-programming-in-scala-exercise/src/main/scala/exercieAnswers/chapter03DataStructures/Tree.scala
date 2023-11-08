package exercieAnswers.chapter03DataStructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
 def size[A](t: Tree[A]): Int = t match {
   case Leaf(_) => 1
   case Branch(l, r) => 1 + size(l) + size(r)
 }

  // Exercise 3.25: get maximum value from tree of int value
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Exercise 3.26: get depth of a tree
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 3.27: Implement map function on a tree
  def map[A, B](t: Tree[A], f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l, f), map(r, f))
  }

  // Exercise 3.28: Implement fold function on a tree
  // f is applied to leaf value
  // g is to combine fold result of two branch
  def fold[A, B](t: Tree[A], f: A => B, g: (B, B) => B): B = t match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l, f, g), fold(r, f, g))
  }
  def sizeViaFold[A](t: Tree[A]): Int = fold(t, (_: A) => 1,  1 + _ + _)
  def depthViaFold[A](t: Tree[A]): Int = fold(t, (_: A) => 1, (d1, d2) => 1 + (d1 max d2))
  def mapViaFold[A, B](t: Tree[A], f: A => B): Tree[B] = fold(t, (a:A) => Leaf(f(a)), Branch(_, _))
  def maximumViaFold(t: Tree[Int]): Int = fold(t, (a: Int) => a, _ max _)
}
