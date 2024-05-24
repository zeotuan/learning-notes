package exercieAnswers.chapter13IO

import scala.io.StdIn.readLine

object IO1 {
  /**
   * Second attempt at IO to allow return meaning value
   * Pros:
   * + IO computations are ordinary value that can be stored and reused like normal variable
   * + IO let us craft more interesting interpreter than the simple unsafeRun baked into IO type
   * + we can improve IO to use non blocking IO while keep the representation identical
   *
   * Cons:
   * + we can cause stackoverflow with this IO implementation if program get large enough.
   * This happen due to how flatMap is implemented. Which just building up unsafeRun function call
   * on the call stack if the number of call get large enough.
   * + the value of IO[A] is completely opaque to user. It's just a lazy identity and there no promise it
   * will actually return a value at a certain time. It might even hang indefinitely
   * + This IO implementation is Blocking
   * */
  sealed trait IO[A] { self =>
    def unsafeRun: A
    def map[B](f: A => B): IO[B] = new IO[B] {
      def unsafeRun: B = f(self.unsafeRun)
    }
    def flatMap[B](f: A => IO[B]): IO[B] = f(self.unsafeRun)
  }


  private object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def unsafeRun: A = a}
    def apply[A](a: => A): IO[A]  = unit(a)
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

    def ref[A](a: A): IO[IORef[A]] = IO { new IORef[A](a) }

    sealed class IORef[A](var value: A) {
      def set(a: A): IO[A] = IO { value = a; a}
      def get: IO[A] = IO { value }
      def modify(f: A => A): IO[A] = get flatMap(a => set(f(a)))
    }

  }

  /** It's now possible to express ReadLine and other IO that return a value */
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0 / 9.0
  def ReadLine: IO[String] = IO(readLine())
  def PrintLine(msg: String): IO[Unit] = IO(println(msg))
  def converter: IO[Unit] = for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()


  // Other interesting Usage
  import IO._

  val echo = ReadLine.flatMap(PrintLine)  // Read then immediately print to stdout

  val readInt: IO[Int] = ReadLine.map(_.toInt) // Read then convert read result to Int

  val readInts: IO[(Int, Int)] = readInt ** readInt // Parse an (Int, Int) by reading 2 line from stdin Unable to resolve using  readInt ** readInt ??

  // Repeat `converter` 5 times,
  val prompts: IO[Unit] = replicateM_(5)(converter)

  // read 10 line from stdin and store in List
  val lines: IO[List[String]] = replicateM(10)(ReadLine)

  val helpstring =
    """
      | The Amazing Factorial REPL, v2.0
      | q - quit
      | <number> - compute the factorial of the given number
      | <anything else> - bomb with horrible error
  """.trim.stripMargin

  def factorial(n: Int): IO[Int] = for {
    acc <- ref(1)
    _ <- foreachM((1 to n ).to(LazyList))(i => acc.modify(_ * 1).void)
    result <- acc.get
  } yield result

  val factorialREPL: IO[Unit] = sequence_(
    PrintLine(helpstring),
    doWhile(ReadLine) { line =>
      when(line != "q") { for {
        n <- factorial(line.toInt)
        _ <- PrintLine(s"factorial $n")
      } yield () }
    }
  )
}