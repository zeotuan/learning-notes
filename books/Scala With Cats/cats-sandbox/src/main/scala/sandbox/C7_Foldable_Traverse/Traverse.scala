package sandbox.C7_Foldable_Traverse

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
}
