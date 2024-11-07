package sandbox.P2_CaseStudy


import cats.{Applicative, Functor, Id, Monoid, Traverse}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt


/**
 * MapReduce: a programming model for processing big data sets in parallel across a clusters of machines.
 * The model is build around:
 * - map phase: same map function we know from Functor type class
 * - reduce phase: same as fold we know from Foldable type class.
 * because of the distributed nature of the model, we lose the ability to guarantee the order of the results.
 * There our reduce function must be associative:
 *   + the fold use monoid to combine the results
 *   + due to the arbitrary number of splits, monoid empty becomes useful as it ensure combine will not affect the result
 *
 *
 *
 * Example of Monoid like Task:
 * - approximate sets: Bloom filter, etc ..;
 * - set cardinality estimators:  HyperLogLog algorithm;
 * - vectors and vector operations: stochastic gradient descent;
 * - quantile estimators: the t‐digest
 * */
object MapReduce {
  import cats.instances.int._
  import cats.instances.vector._
  import cats.instances.future._
  import cats.instances.list._
  import cats.syntax.foldable._
  import cats.syntax.traverse._
  import cats.Foldable

  Foldable[Vector].foldMap(Vector(1, 2, 3))(identity) // 6 - addition int monoid
  Foldable[Vector].foldMap(Vector(1, 2, 3))(_.toString) // "123" - concatenation string monoid

  def foldMapVectorSequential[A, B: Monoid](va: Vector[A])(f: A => B): B = va.foldLeft(Monoid[B].empty) { case (acc, a) => Monoid[B].combine(acc, f(a)) }
//  def foldMap[F[_]: Functor, A, B: Monoid](fa: F[A])(f: A => B): B = Functor[F].map(fa)

  val future1 = Future {
    (1 to 100).foldLeft(0)(_ + _)
  }

  val future2 = Future {
    (1 to 100).foldLeft(0)(_ + _)
  }

  val future3 = future1.map(_.toString)

  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b

  Future.sequence(List(future1, future2))
  List(future1, future2).sequence

  Monoid[Future[Int]].combine(Future(1), Future(2))


  val dataGroup =(1 to 10).grouped(3).toList
  def parallelFoldMap[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    val groups = values.grouped(groupSize)
    // map then sequence which can be replaced by traverse in second implementation
    val futures = groups.map(group => Future { foldMapVectorSequential(group)(func) })
    Future.sequence(futures).map { list =>
      list.foldLeft(Monoid[B].empty)(Monoid[B].combine)
    }
  }

  def parallelFoldMap2[A, B: Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val numCores = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => {
          Future(group.foldMap(func)) // Future[B]
      })
      .map(_.combineAll)
//    values
//      .grouped(groupSize)
//      .toVector
//      .traverse(group => Future(group.foldMap(func)))
//      .map(_.foldMap(identity))
  }

}

