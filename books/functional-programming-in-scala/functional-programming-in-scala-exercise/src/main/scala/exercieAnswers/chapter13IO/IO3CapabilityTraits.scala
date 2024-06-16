package exercieAnswers.chapter13IO

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par

import scala.io.StdIn.readLine
import scala.util.Try

/**
 * Alternative to first constructing the I/O action as an free algebra, we can also model it
 * using capability trait parameterized by a target monad and define each primitive operation
 * as a method returning a value of the target Monad
 * */
object IO3CapabilityTraits {

  /**
   * the readLn and printLn operation on the Console trait provide
   * constructor of the primitive console operation in the target monad
   * */
  trait Console[F[_]] {
    def readLn: F[Option[String]]
    def printLn(line: String): F[Unit]
  }

  object Console {
    // instance of the Console trait for each target Monad we need to support
    // These instance are analogous to the interpreter we wrote for the free algebras
    // but we not constructing the Free program then later interpret it but directly create
    // instance of our target monad
    implicit val parConsole: Console[Par] = new Console[Par] {
      override def readLn: Par[Option[String]] = Par.lazyUnit(Try(readLine()).toOption)

      override def printLn(line: String): Par[Unit] = Par.lazyUnit(println(line))
    }

    implicit val thunkConsole: Console[Function0] = new Console[Function0] {
      override def readLn: () => Option[String] = () => Try(readLine()).toOption

      override def printLn(line: String): () => Unit = () => println(line)
    }

    implicit val thunkMonad = new Monad[Function0] {
      override def flatMap[A, B](a: () => A)(f: A => () => B): () => B = () => f(a())()
      override def unit[A](a: => A): () => A = () => a
    }

    implicit val ParMonad = new Monad[Par] {
      override def unit[A](a: => A): Par[A] = Par.unit(a)
      override def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(a)(f) }
    }

    /**
     * greet can be used by any monad with a target monad and Console instance
     * */
    def greet[F[_]](c: Console[F], m: Monad[F]): F[Unit] = {
      import m._
      for {
        _ <- c.printLn("What's your name?")
        n <- c.readLn
        _ <- n match {
          case Some(name) => c.printLn(s"Hello, $name!")
          case None => c.printLn(s"Fine, be that way.")
        }
      } yield ()
    }

    def runGreetThunk(): Unit = greet(thunkConsole, thunkMonad)

    trait Files[F[_]] {
      def readLines(file: String): F[List[String]]

      def writeLines(file: String, lines: List[String]): F[Unit]
    }

    object Files {
      implicit val thunkFiles: Files[Function0] = new Files[Function0] {
        def readLines(file: String): () => List[String] = () => List("line 1", "line 2")

        def writeLines(file: String, lines: List[String]): () => Unit = () => ()
      }

      /**
       *  Files and Console capabilities can be compose due to sharing the same target monad
       *  without having to union or having explicit composition.
       *  Capability trait approach therefore is much simpler.
       *  THe stack safety however, is entirely dependence on the monad flatMap implementation.
       *   we can choose to use a stack-safe monad Free[Par, x] instead of simple Function[0]
       *  */
      def cat[F[_]](file: String)(c: Console[F], f: Files[F], m: Monad[F]): F[Unit] = {
        import m._
        f.readLines(file).flatMap { lines =>
          c.printLn(lines.mkString("\n"))
        }
      }

      def catThunk(): Unit = cat("hello")(thunkConsole, thunkFiles, thunkMonad)
    }


    /**
     * Console imbues type constructor F with the ability to perform console operation
     * Monad imbue F with sequencing ability (flatMap). Instead of passing Free algebra
     * around we pass abstract effectful program.
     * */
  }
}
