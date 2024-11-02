package sandbox.P2_CaseStudy

import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.Traverse
import cats.Applicative
import cats.Id

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
 *
 *   + the fold use combine from a monoid to combine the results
 * */
object MapReduce {

}

