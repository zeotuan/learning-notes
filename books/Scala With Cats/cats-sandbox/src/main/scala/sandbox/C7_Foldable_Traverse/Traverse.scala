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
}
