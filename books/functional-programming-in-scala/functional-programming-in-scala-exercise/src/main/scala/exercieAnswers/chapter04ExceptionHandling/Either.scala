package exercieAnswers.chapter04ExceptionHandling

// Suppress native Option and Either to implement our own
import scala.util.control.NonFatal
import scala.{Either => _, Option => _}

// Exercise 4.6: Implement Either type and common helper function to enable for comprehension
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }
  def orElse[EE >: E,  B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }

  // Implement using flatmap and map
  // One limitation of this Implementation is that it can only report one error while multiple error could occur
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A,B) => C): Either[EE, C] = this.flatMap(a => b.map(bb => f(a, bb)))
  // For comprehension with same Implementation
  def map2ForComprehension[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    a <- this
    bb <- b
  } yield f(a, bb)


  // This implementation allow us to get List of Error. However, if we call map2Both with two Either
  // Type which is the result of another map2Both, we will get a "deeply" nested List of error
  // To fix this, refer to map2All
  def map2Both[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[List[EE], C] = (this, b) match {
    case (Right(a), Right(b)) => Right(f(a,b))
    case (Left(e), Right(_)) => Left(List(e))
    case (Right(_), Left(e)) => Left(List(e))
    case (Left(e), Left(e1)) => Left(List(e,e1))
  }
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Option {
  def mean(xs: List[Double]): Either[Throwable, Double] = catchNoneFaltable(xs.sum/xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Double] = catchNoneFaltable(x/y)

  def catchNoneFaltable[A](a: => A): Either[Throwable, A] = try {
    Right(a)
  } catch {
    case NonFatal(t) => Left(t)
  }

  // Exercise 4.7: Implement sequence and traverse
  // traverse using foldRight
  def traverse[E,A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  // We can also utilize map2 and pattern matching to implement traverse
  def traverse_2[E,A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse_2(t)(f))(_ :: _)
  }

  // similar to Option Sequence implementation, we can utilize traverse with an Identity function to implement this
  def sequence[E,A](as: List[Either[E, A]]): Either[E, List[A]] = traverse(as)(a => a)
  def sequenceViaPatternMatch[E,A](as: List[Either[E, A]]): Either[E, List[A]] = as match {
    case Nil => Right(Nil)
    case h::t => h flatMap (hh => sequenceViaPatternMatch(t) map (hh :: _))
  }

 def sequenceViaFoldRight[E,A](as: List[Either[E, A]]): Either[E, List[A]] = as.foldRight[Either[E, List[A]]](Right(Nil))((a,b) => a.map2(b)(_ :: _))


  // This Implementation now require that the Either Type should already have List of error for Left case
  // We can now preserve the error type
  def map2All[EE, AA, B, C](a: Either[List[EE], AA], b: Either[List[EE], B])(f: (AA, B) => C): Either[List[EE], C] = (a, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(e), Right(_)) => Left(e)
    case (Right(_), Left(e)) => Left(e)
    case (Left(e), Left(e1)) => Left(e ++ e1)
  }

  // Traverse all can be implement by using map2All instead of map2
  //TODO: synchronize all map2 and map2All Implementation for clarity
  def traverseAll[E, A, B](as: List[A])(f: A => Either[List[E], B]): Either[List[E], List[B]] = as.foldRight[Either[List[E], List[B]]](Right(Nil))((a, b) => map2All(f(a), b)(_ :: _))
  def sequenceAll[E,A](as: List[Either[List[E], A]]): Either[List[E], List[A]] = traverseAll(as)(a => a)


  // We can introduce new Validated Type which specifically support List of Error
  // However, Another approach is to make map2 support combining error
  // with this, EE could be of type List[String] and combineErr = _ ++ _
  def map2AllCombine[EE, AA, B, C](a: Either[EE, AA], b: Either[EE, B])(f: (AA, B) => C, combineErr: (EE, EE) => EE): Either[EE, C] = (a, b) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case (Left(e), Right(_)) => Left(e)
    case (Right(_), Left(e)) => Left(e)
    case (Left(e), Left(e1)) => Left(combineErr(e, e1))
  }

  // Update traverAllCombine to suppoort map2AllCombine
  def traverseAllCombine[E,A, B](as: List[A])(f: A => Either[E, B], combineErr: (E, E) => E): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => map2AllCombine(f(a), b)(_ :: _, combineErr))
  def sequenceAllCombine[E,A](as: List[Either[E, A]], combineErr: (E, E) => E): Either[E, List[A]] = traverseAllCombine(as)(a => a, combineErr)

}
