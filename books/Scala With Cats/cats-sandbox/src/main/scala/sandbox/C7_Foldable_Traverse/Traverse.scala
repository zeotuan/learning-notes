package sandbox.C7_Foldable_Traverse

import cats.Applicative
import cats.implicits.{catsSyntaxApplicativeId, catsSyntaxTuple2Semigroupal}

import scala.concurrent.Future
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * higher level tool that leverage Applicative to provide
 * more lawful, convenient pattern of iteration
 * with Traverse we can turn an F[G[A]] into a G[F[A]] for any F with an instance of
 * Traverse and any G with an instance of Applicative
 * */
object TraverseSample {
  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )
  // Problem is we have to create and combine future every iteration
  val futureList =  hostnames.foldLeft(Future(List.empty[Int])) {
    (accum, host) =>
      val upTime = Future(host.length * 60)
      for {
        acc <- accum
        up <- upTime
      } yield acc :+ up
  }
  Await.result(futureList, 1.second) // List(1020, 960, 840)


  Future.traverse(hostnames)(host => Future(host.length * 60))

  // look like traverse perform exactly the same as the manual version
  // thought it is more generic and can be used with any type constructor
  def traverse[A, B](values: List[A])
                    (func: A => Future[B]): Future[List[B]] =
    values.foldLeft(Future(List.empty[B])) { (accum, host) =>
      val item = func(host)
      for {
        accum <- accum
        item <- item
      } yield accum :+ item
    }

  def sequence[A](values: List[Future[A]]): Future[List[A]] = traverse(values)(identity)

  // by substituting the initial value with Applicative.pure
  // and the combining function with SemiGroupal.combine
  // we now have A more generic version of traverse which can be used with any Applicative
  def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) {
      (accum, item) =>(accum, func(item)).mapN((a, b) => a :+ b)
    }

  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  val totalUptime2 = Await.result(listTraverse(hostnames)(host => Future(host.length * 60)), 1.second)
  // using default SemiGroupal instance for Option return None here since flatMap is sequential
  listTraverse(List(1, 2, 3))(n => if (n % 2 == 0) Some(n) else None)
  // we get Some(List(2, 4, 6) here since all element divisible by 2
  listTraverse(List(2, 4, 6))(n => if (n % 2 == 0) Some(n) else None)


  // Traverse with Validated
  import cats.data.Validated
  import cats.instances.list._

  type ErrorOr[A] = Validated[List[String], A]

  def process(inputs: List[Int]): ErrorOr[List[Int]] = listTraverse(inputs) { n =>
    if (n % 2 == 0) Validated.Valid(n)
    else Validated.Invalid(List(s"$n is not even"))
  }
}

trait RawTraverse[F[_]] {
  def traverse[G[_]: Applicative, A, B](inputs: F[A])(func: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](inputs: F[G[A]]): G[F[A]] = traverse(inputs)(identity)
}

object CatTraverseSample {
  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._ // for Traverse

  val totalUptime = Traverse[List].traverse(List("alpha.example.com", "beta.example.com", "gamma.demo.com"))(n => Future(n.length * 60))
  val number2 = Traverse[List].traverse(List(1,2,3))(n => if (n % 2 == 0) Some(n) else None)

  // even sorter approach  via syntax
  import cats.syntax.traverse._
  List("alpha.example.com", "beta.example.com", "gamma.demo.com").traverse(n => Future(n.length * 60))
  List(1, 2, 3).traverse(n => if (n % 2 == 0) Some(n) else None)

}