package sandbox.C4_Monad

import cats.data.WriterT

/** Writer Monad lets us carry a log along with a computation */
object WriterMonad {
  import cats.data.Writer // for Writer
  import cats.instances.vector._ // for monoid
  import cats.syntax.applicative._ // for pure

  // Writer is implemented in terms of WriterT which is why the Type is
  // WriterT[cats.Id, Vector[String], Int] instead of Writer[Vector[String], Int]
  // type Writer[W, A] = WriterT[cats.Id, W, A]
  type Writer[W, A] = WriterT[cats.Id, W, A]
  val testWriter: Writer[Vector[String], Int] = Writer(Vector(
    "It was the best of times",
    "It was the worst of times"
  ), 1859)

  // we can create Writer with only the log or the value
  type Logged[A] = Writer[Vector[String], A]
  val loggedTest = 123.pure[Logged]

  import cats.syntax.writer._

  val onlyLog = Vector("msg1", "msg2", "msg3").tell

  val aWriter = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val a1Writer = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult = aWriter.value
  val aLog = aWriter.written
  val (aLog1, aResult1) = aWriter.run


  /**
   * writer.map preserve the log while writer.flatMap appends the logs
   * from the source writer and the result of the sequencing func.
   * Therefore, it's good practice to use Log Type with an efficient append and concat ops
  */

  val writer1 = for {
    a <- 20.pure[Logged]
    _ <- Vector("msg4", "msg5", "msg6").tell
    b <- 456.writer(Vector("msg7", "msg8", "msg9"))
  } yield a + b

  val (writer1Log, aWriter1Result) = writer1.run

  val writer2 = writer1.mapWritten(_.map(_ + "!")) // map ver the log
  val writer3 = writer2.bimap( // map over both log and result
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  val writer4 = writer3.mapBoth((log, res) => // also map over both log and result
    (log :+ "!").map(_.toLowerCase) -> res * 10
  )
  val writer5 = writer4.reset // clear the log
  val writer6 = writer4.swap // swap the log and result

  def slowly[A](body: => A): A = try body finally Thread.sleep(50)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println("fact: " + n + " -> " + ans)
    ans
  }

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.seconds)
  // we will see potentially interleaved logs from the two futures making it hard to debug

  def factorialWriter(n: Int): Logged[Int] = for {
    ans <- if (n == 0) 1.pure[Logged] else slowly(factorialWriter(n - 1).map(_ * n))
    _ <- Vector(s"fact: $n -> $ans").tell
  } yield ans

}

