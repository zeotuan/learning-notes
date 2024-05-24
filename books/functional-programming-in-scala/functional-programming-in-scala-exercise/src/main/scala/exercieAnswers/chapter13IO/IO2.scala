package exercieAnswers.chapter13IO

import scala.annotation.tailrec

object IO2 {

  /**
   * Instead of recursive call to unsafeRun, we create a data structure
   * which allow us to passed back the control flow and write our own interpreter
   * */
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def map[B](f: A => B): IO[B] = flatMap(f andThen(Return(_)))
    /**
     * tail recursive interpreter which traverse the structure and perform the effect
     * we make the program control explicit in our IO data type.
     * When this is run. It's determine whether the program is requesting to execute some
     * effect with a Suspend(s) or it want to call a subroutine with Flatmap(x,f).
     * Instead of making use of the callstack, unsafeRun will call x() then continue by calling
     * f on the result of x(). f will return  either Suspend, Return or Flatmap transferring back
     * control to unsafeRun. Our IO program is therefore a kind of coroutine that executes cooperatively with unsafeRun.
     * It continually makes either Suspend or FlatMap IO request and each time it does so, it suspends its own execution
     * and return control to unsafeRun. It's actually unsafeRun that drive the execution forward one suspension at a time.
     * unsafeRun is sometimes called trampoline and the overall technique of returning control to a single loop to eliminate
     * stack is called trampolining
     * */
    @tailrec
    final def unsafeRun(): A = this match {
      case Return(a) => a
      case Suspend(r) => r()
      case FlatMap(x, f) =>  x match {
        case Return(a) => f(a).unsafeRun()
        case Suspend(r) => f(r()).unsafeRun()
        case FlatMap(y, g) => (y flatMap(a => g(a) flatMap f)).unsafeRun()
      }
    }
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]


  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = Return(a)
    def flatMap[A, B](a: IO[A])(f: A => IO[B]): IO[B] = a flatMap f
    // the evaluation of these below function is delayed until the thunk passed to the internal suspend is forced
    def suspend[A](a: => IO[A]): IO[A] = Suspend(() => a).flatMap(identity) // delay computations of IO[A] until the return program is interpreted
    def apply[A](a: => A): IO[A] = suspend(Return(a)) // delay computation of a until the program is interpreted
  }

  def PrintLine(msg: String): IO[String] = IO(println(msg))

  val p = IO.forever(PrintLine("still going ..."))

  val actions: LazyList[IO[String]] = LazyList.fill(1000000)(PrintLine("still going ..."))

  val f: Int => IO[Int] = (x: Int) => Return(x)
  val g: Int => IO[Int] = LazyList
    .fill(1000000)(f)
    .foldLeft(f)(
      (a, b) => x => IO.suspend(a(x).flatMap(b))
    )

  // val composite: IO[Unit] = actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }
}
