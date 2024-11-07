package sandbox.C4_Monad

import cats.implicits.catsSyntaxApplicativeId

object CatStateMonad {
  import cats.data.State
  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }

  // State.run return an Eval
  val (state, result) = a.run(10).value

  val justTheState = a.runS(10).value
  val justTheResult = a.runA(10).value

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield a + b

  val (bothState, bothResult) = both.run(10).value


  // Generally, State Monad is used to represent a computation that carries some state along
  // each step of the computation is a instance that can be composed using monadic operations
  // There are several convenience constructors for creating State instances


  // get extracts the state as the result
  val  getDemo = State.get[Int]
  getDemo.run(10).value // (10, 10)

  // set updates the state and return unit as the result
  val setDemo = State.set[Int](30)
  setDemo.run(10).value // (30, ())

  // pure ignores the current state and returns a supplied result
  val pureDemo = State.pure[Int, String]("Result")
  pureDemo.run(10).value // (10, "Result")

  // very similar to get put apply a transformation on the return sstate
  val inspectDemo = State.inspect[Int, String](_ + "!")
  inspectDemo.run(10).value // (10, "10!")

  // similar to set but modify the exist state via a function
  val modifyDemo = State.modify[Int](_ + 1)
  modifyDemo.run(10).value // (11, ())

  val program: State[Int, (Int, Int, Int)] = for {
    a <- State.get[Int]
    _ <- State.set[Int](a + 1)
    b <- State.get[Int]
    _ <- State.modify[Int](_ + 1)
    c <- State.inspect[Int, Int](_ * 1000)
  } yield (a, b, c)


  object PostOrderCal {
    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

    def operand(num: Int): CalcState[Int] = State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

    def operator(func: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Fail!")
    }

    val addOneTwoProgram = for {
      _ <- evalOne("1")
      _ <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    addOneTwoProgram.run(Nil).value // 3

    def evalAll(input: Seq[String]): CalcState[Int] = input.foldRight(0.pure[CalcState]) { (sym, acc) =>
      acc.flatMap(_ => evalOne(sym))
    }

    val multiCalculation = evalAll(List("1", "2", "+", "3", "*")) // (1 + 2) * 3
    multiCalculation.run(Nil).value // 9

    val stepCalculation = for {
      _ <- evalAll(Seq("1", "2", "+")) // 1 + 2
      _ <- evalAll(Seq("3", "4", "+")) // 3 + 4
      ans <- evalOne("*") // (1 + 2) * (3 + 4)
    } yield ans


  }
}
