package sandbox.C7_Foldable_Traverse

import cats.{Eval, Monoid}

object FoldableSample {
 // map, flatMap, filter, sum in terms of foldLeft
 def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List.empty[B])((acc, a) => acc :+ f(a))
 def flatMap[A, B](list: List[A])(f: A => List[B]) = list.foldLeft(List.empty[B])((acc, a) => acc ++ f(a))
 def filter[A](list: List[A])(f: A => Boolean) = list.foldLeft(List.empty[A])((acc, a) => if (f(a)) acc :+ a else acc)
 def sum[A](list: List[A])(implicit m: Monoid[A]): A = list.foldLeft(m.empty)(m.combine)

 // map, flatMap, filter, sum in terms of foldRight order is a bit different so we need to append to the head of the list
 def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
 def flatMap[A, B](list: List[A])(f: A => List[B]) = list.foldRight(List.empty[B])((a, acc) => f(a) :: acc)
 def filter[A](list: List[A])(f: A => Boolean) = list.foldRight(List.empty[A])((a, acc) => if (f(a)) a:: acc else acc)
 def sum[A](list: List[A])(implicit m: Monoid[A]): A = list.foldRight(m.empty)(m.combine)

 import cats.Foldable
 import cats.instances.list._ // for Foldable

 val ints = List(1, 2, 3)
 Foldable[List].foldLeft(ints, 0)(_ + _) // 6 same as ints.foldLeft(0)(_ + _)

 object FoldRightSample {
  import cats.instances.lazyList._
  // Foldable define foldRight differently from foldLeft, in terms of Eval monad
  // This mean foldRight is always stack safe even when the collection definition is not
  def foldRight[F[_], A, B](fa: F[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

  val bigData = (1L to 1000000L).to(LazyList)
  val eval: Eval[Long] = Foldable[LazyList].foldRight(bigData, Eval.now(0L)) {
   (a, acc) => acc.map(_ + a)
  }
 }

 /**
  * Foldable provides us with a host of useful methods: find, exists, forall, toList, isEmpty, nonEmpty, foldLeft, foldRight, and more.
  * */
 Foldable[Option].nonEmpty(Some(1))
 Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)

 /**
  * Foldable provide some methods that require Monoid instance
  * - combineAll: combine all elements in the collection using the Monoid instance
  * - foldMap: more generic version of combineAll which let's you apply some mapping function before combining
  * */
 import cats.instances.int._

 Foldable[List].combineAll(List(1, 2, 3)) // 6 - perform sum since it's the default Monoid for Int
 Foldable[List].foldMap(List(1, 2, 3))(_.toString) // "123" - convert each element to string then concatenate them since it's the default Monoid for String

 /**
  * Scala will use an instance of Foldable is the method isn't explicitly available on the receiver
  * List(1, 2, 3).foldLeft(0)(_ + _) // this use foldLeft defined on list
  *
  * this one use Foldable instance
  * */
 def sum[F[_]: Foldable](values: F[Int]): Int = Foldable[F].foldLeft(values, 0)(_ + _)
}


