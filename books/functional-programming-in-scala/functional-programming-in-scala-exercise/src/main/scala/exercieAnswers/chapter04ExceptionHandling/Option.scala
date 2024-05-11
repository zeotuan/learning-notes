package exercieAnswers.chapter04ExceptionHandling

// Suppress native Option and Either to implement our own
import scala.{Option => _, Either => _}


// Exercise  4.1: Implement Option class with helper function
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
  def flatMapViaPatternMatch[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_)) getOrElse ob
  def orElseViaPatternMatch[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
}
case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

object Option {
  // Exercise 4.2 Implement function to get variance of a list
  // Helper function to calculate mean of a list
  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // lift allow us to turn ordinary function into function that support Option type
  def lift[A, B](f: A => B): Option[A] => Option[B] = _.map(f)

  val absO: Option[Double] => Option[Double] = lift(math.abs)
  val ex1  = absO(None)
  val ex2  = absO(Some(-1))


  // Exercise 4.3: Generic Map function to combine two Option
  // This is an alternative to lip allowing us to use ordinary function on option type
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aa => b.map(bb => f(aa,  bb)))
  val exOptionSum = map2(Some(1), Some(2))(_ + _)
  val exOptionProduct = map2(Some(2), None)(_ * _)

  // Exercise 4.4: write a sequence function that combines a list of Options into on Options containing List
  // recursive solution by pattern match on original list of option
  // if have head and tail then flatMap on head to get the value
  // Use flatMap, where you recurse on tail then append head to it
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l match {
    case Nil => Some(Nil)
    case h::t => h flatMap (hh => sequence(t) map (hh :: _))
  }
  def sequence[A](l: List[Option[A]]): Option[List[A]] = l.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a,b)( _ :: _))

  // Exercise 4.5: Implement a traverse function that map a function (might fail) to a list. If any element fail then return None
  // easiest implementation is to map the sequence. But it is not the most efficient because it require traversing the list twice
  def traverseViaMapSequence[A, B](l: List[A], f: A => Option[B]): Option[List[B]] = sequence(l.map(f))

  // More efficient Implementation using foldRight
  def traverse[A, B](l: List[A], f: A => Option[B]): Option[List[B]] = l.foldRight[Option[List[B]]](Some(Nil))((a, b) => map2(f(a), b)(_ :: _))

  def traverseViaPatternMatch[A, B](l: List[A], f: A => Option[B]): Option[List[B]] = l match {
    case None => Some(Nil)
    case h :: t => map2(f(h), traverseViaPatternMatch(t, f))(_ :: _)
  }

  // scala for-comprehension implementation of map2 
  // it automatically expand and apply flatmap and map call
  def map2ForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  // as you can see, sequence can also be implemented using traverse
  // We simply need to map since l is already a list of Option
  // we can simply using an identify function
  def sequenceViaTraverse[A](l: List[Option[A]]): Option[List[A]] = traverse(l, (a: Option[A]) => a)

}
