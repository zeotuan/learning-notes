package sandbox.C4_Monad

object CatEvalMonad {
  import cats.Eval
  val now = Eval.now(math.random + 1000) // eager and memoized similar to val
  val later = Eval.later(math.random + 2000) // lazy and memoized similar to lazy val
  val always = Eval.always(math.random + 3000) // always lazy without memoization similar to def

  val greets = Eval
    .always {
      print("s1")
      "Hello"
    }
    .map { str =>
      println("s2")
      s"$str World"
    }.map { str =>
      println("s3")
      s"$str!"
    }

  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 } // this will be calculated immediately and memoized even is ans.value is not called
    b <- Eval.always { println("Calculating B"); 2 } // this will always be re-evaluated on ans.value
  } yield {
    println("Adding A and B")
    a + b
  }

  // we can use memoize to memoize a chain of compuration
  greets.memoize.map { str =>
      println("s4")
      s"$str What's up!"
    }

  // map and flatMap of Eval are trampolined which  make them stake safe
  // Eval is useful to ensure stack safety but it come with the cost
  // It create a chain of functions object on the heap to avoid consuming stack space
  // accessing heap is not as fast as and we are still limited by heap size
  def factorial(n: BigInt): Eval[BigInt] = if (n == 1) {
    Eval.now(n)
  } else {
    // defer evaluation of an existing Eval
    Eval.defer(factorial(n - 1).map(_ * n))
  }

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail =>
      Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil =>
      acc
  }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = foldRightEval(as, Eval.now(acc)) { (a, b) => b.map(fn(a, _)) }.value
}
