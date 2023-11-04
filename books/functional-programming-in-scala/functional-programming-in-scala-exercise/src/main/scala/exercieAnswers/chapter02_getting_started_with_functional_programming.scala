package exercieAnswers

import scala.annotation.tailrec

object TailRecursionAndHigherOrderFunction {

  // higher order function. Similar to most other language
  def formatResult(name: String, x: Int, f: Int => Int) = "The %s of %d is %d".format(name, x, f(x))


  def abs(n: Int) = if (n < 0) -n else n
  // tail recursive factorial implementation using pure function
  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n < 0)
      acc
    else
      go(n - 1, acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, cur: Int, next: Int): Int = if (n <= 0)
      cur
    else
      go(n-1, next, cur + next)

    go(n, 0, 1)
  }

  val absString = println(formatResult("abs", -10, abs))
  val factotialString = println(formatResult("factorial", -10, factorial))
  val fibString = println(formatResult("fib", -10, fib))
  val anonString = println(formatResult("anon", -10, x => x + 1))
}

// Polymorphic or in other word Generic function
object PolymorphicFunction {
  def findFirst[A](ss: Seq[A], key: A): Int = {
    @tailrec
    def go(n: Int): Int = n match {
      case n if n >= ss.length => -1
      case n if ss(n) == key => n
      case _ => go(n+1)
    }
    go(0)
  }

  assert(findFirst[String](Seq("a", "b", "d", "c"), "c") == 3)
  assert(findFirst[Int](Seq(1, 2, 3, 4, 5, 6), 3) == 2)

  def isSorted[A](ss: Seq[A], f: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(n: Int): Boolean = n match {
      case n if n >= ss.length - 1 => true
      case n if f(ss(n), ss(n + 1)) => false
      case _ => go(n+1)
    }

    go(0)
  }
  val isAsc = isSorted(Seq(1,2,3,4,5,6), (x: Int, y: Int) => x < y)
 assert(isAsc)
  val isDesc = isSorted(Seq(6,5,4,3,2,1), (x: Int, y: Int) => x > y)
  assert(isDesc)

  // Due to type constrain, many Polymorphic functions only have one implementation
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =(b: B) => f(a, b)
  val smallerThan10 = partial1(10, (x: Int, y: Int) => x  > y)
  assert(smallerThan10(2))


  // curry implementation
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a,b)
  val greater = curry((x: Int, y: Int) => x < y)
  val greaterThan1 = greater(1)
  assert(greaterThan1(2))

  // uncurried implementation
  // normally you would use Function.uncurried()
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a,b) => f(a)(b)
  val isGreaterUncurried = uncurry(greater)
  assert(isGreaterUncurried(10, 20))

  // a higher order function that compose other function
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
  val composeA = compose[Int, Double, String]((x: Double) => (x + 2).toString, (y: Int) => y/2)
  assert(composeA(10) == '7')
}
