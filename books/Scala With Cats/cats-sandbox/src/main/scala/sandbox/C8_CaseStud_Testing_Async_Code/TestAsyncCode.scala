package sandbox.C8_CaseStud_Testing_Async_Code

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

trait UptimeClient[F[_]] {
  def getUptime(hostName: String): F[Int]
}

trait RealUptimeClient extends UptimeClient[Future] {
  def getUptime(hostName: String): Future[Int]
}

trait IdUptimeClient extends UptimeClient[Id] {
  def getUptime(hostName: String): Id[Int]
}

trait Tes

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  // without importing syntax
  def getTotalUptime(hostNames: List[String]): F[Int] = Applicative[F]
    .map(
      Traverse[List].traverse(hostNames)(client.getUptime)
    )(_.sum)

  // with importing syntax
  def getTotalUptime2(hostNames: List[String]): F[Int] =
      hostNames.traverse(client.getUptime).map(_.sum)
}

class TestUptimeClientId(hosts: Map[String, Int]) extends IdUptimeClient {
  def getUptime(hostName: String): Id[Int] = hosts.getOrElse(hostName, 0)
}

class TestUptimeClientFuture(hosts: Map[String, Int]) extends RealUptimeClient {
  def getUptime(hostName: String): Future[Int] = Future.successful(hosts.getOrElse(hostName, 0))
}

/**
 * Here is a great example of meta programming
 * Where we can inject F to the UptimeClient and UptimeService
 * and change the behaviour of the service
 * From async to sync
 *
 * Type classes like Functor, Applicative, Monad, Traverse, Foldable, Semigroupal
 * provides abstract implementation  of patterns such as mapping, zipping, sequencing, folding, iteration
 * The mathematical laws ensure that they work with consistent behaviour
 * */
object TestUptimeClientNonGeneric {
  def testTotalUptimeId() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientId(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  def testTotalUptimeFuture() = {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClientFuture(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(Await.result(actual, 1.second) == expected)
  }
}


