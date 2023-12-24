package exercieAnswers.chapter08PropertyTesting

import exercieAnswers.chapter05Laziness.LazyList
import exercieAnswers.chapter06State.{RNG, State}
import exercieAnswers.chapter08PropertyTesting.Prop.{FailedCase, Falsified, MaxSize, Passed, Proved, Result, SuccessCount, TestCases, forAll}
import exercieAnswers.chapter07Parallelism.Par

import java.util.concurrent.Executors

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  self =>

  // Exercise 8.9 Implement && and || operator
  def &&(that: Prop): Prop = Prop {
    (max, n, rng) => tag("and-left").run(max, n, rng) match {
      case Passed | Proved => that.tag("and-right").run(max, n, rng)
      case x => x
    }
  }

  def ||(that: Prop): Prop = Prop {
    (max, n, rng) => tag("or-left").run(max, n, rng) match {
      case Falsified(msg, _) => that.tag("or-right").tag(msg).run(max, n, rng)
      case x => x
    }
  }

  def tag(msg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(e, c) => Falsified(s" $msg\n $e", c)
      case x => x
    }
  }
  def check(maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Result = run(maxSize, testCases, rng)


}
object Prop {
  type MaxSize = Int
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified: Boolean = false
  }
  case class Falsified(failure: FailedCase, success: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }
  case object Proved extends Result {
    override def isFalsified: Boolean = false
  }

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(r => Some(g.sample.run(r))) // generate infinite lazy list of random value by sampling the generator

  def buildMsg[A](s: A, e: Exception): String = s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, _,rng) => randomLazyList(as)(rng).zip(LazyList.from(0)).take(n).map { // Create LazyList of Pair (a: A, i: Int) value -> index
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i) // apply f and record Result
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed) // Check if any result is Falsified
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n - 1) / max + 1
      val props: LazyList[Prop] = LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f)) // generate property per size
      val prop: Prop = props.map(p => Prop { (m, _, rng) => p.run(m, casesPerSize, rng) }).toList.reduce(_ && _) // combine into single property
      prop.run(max, n, rng)
    }
  }

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit= {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests: \n $msg.")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }
  }

  def verify(p: => Boolean): Prop = Prop { (_, _, _) => if (p)  Passed else Falsified("()", 0)}

  val executors = Gen.weighted(Gen.choose(1,4).map(Executors.newFixedThreadPool) -> 0.75, Gen.unit(Executors.newCachedThreadPool) -> 0.25)
  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors.map2(g)(_ -> _)) { case (s, a) => f(a)(s).get }

  def forAllPar1[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors ** g) { case (s, a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors ** g) { case s ** a => f(a)(s).get }

  def verifyPar(p: Par.Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)
}


case class Gen[+A](sample: State[RNG, A]) {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???


  // Exercise 8.6: Implement flatMap and a more dynamic listOfN version using it
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](g: Gen[B])(f: (A,B) => C) = Gen(sample.map2(g.sample)(f))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => listOfN(s))

  def unsized = SGen(_ => this)

  def list: SGen[List[A]] = Gen.listOf(this)
  def nonEmptyList: SGen[List[A]] =  Gen.listOf(this, 1)

  def **[B](gb: Gen[B]): Gen[(A,B)] = map2(gb)(_ -> _)

}

object Gen {

  /**
   * Exercise 8.4: choose generate a random integer in range [start, stopExclusive)
   * */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegative).map(n => start + n % (stopExclusive - start)))

  // Exercise 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

  // Exercise 8.7: Implement union which combine two Gen of same type into 1 by pulling value from each generator based on equal likelyhood
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  // Exercise 8.8: a weighted version of union
  // generate a probability in range [0,1)
  // select g1 if it is less than normalized weighting
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs) // this is the probability of choosing g1 over g2
    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample))
  }

  def listOf[A](g: Gen[A], min: Int = 0): SGen[List[A]] = SGen(n => g.listOfN(n.max(min)))

  def genStringfn[A](g: Gen[A]): Gen[String => A] = Gen(State((rng: RNG) => {
    val (seed, rng2) = rng.nextInt // use rng to produce seed so we get a new function each time
    // use hashcode of the string to perturb the rng state. This make sure that any unique
    // String value will produce a unique function
    val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
    (f, rng2)
  }))

  def stringN(n: Int): Gen[String] = listOfN(n, choose(0, 127)).map(_.map(_.toChar).mkString)

  val string: SGen[String] = SGen(stringN)
}

case class SGen[+A](g: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = g(n)

  def map[B](f: A => B): SGen[B] = SGen(this(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(n => this(n).flatMap(f(_)(n)))

  def **[B](sg: SGen[B]): SGen[(A, B)] = SGen(n => this(n) ** sg(n))
}

object ExampleTest {
  // Test List.max
  val maxProp = Prop.forAll(Gen.choose(-10, 10).nonEmptyList) { l => {
    val max = l.max
    l.forall(_ <= max)
  }}


  // Test for List.sort
  val sortedProp = Prop.forAll(Gen.choose(-10, 10).list) {l => {
    val ls = l.sorted
    l.isEmpty || l.tail.isEmpty || !ls.zip(ls.tail).exists { case (cur, next) => cur > next} // each subsequent element must be bigger than the previous
  }}

  // Test parallel computation for unit(1).map(_ + 1) == unit(2)
  val executor = Executors.newCachedThreadPool()

  // incremental improvement by introducing helper func
  val p1 = Prop.forAll(Gen.unit(Par.unit(1))) { pi => Par.map(pi)(_ + 1)(executor).get == Par.unit(2)(executor).get}
  val p2 = Prop.verify(Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2))(executor).get)
  val p3 = Prop.verifyPar(Par.equal(Par.map(Par.unit(1))(_ + 1), Par.unit(2)))

  // Verify unit(x).map(f) == unit(f(x))
  // y.map(x => x) == y
  val gpy: Gen[Par.Par[Int]] = Gen.choose(0, 10).map(Par.unit)
  val p4 = Prop.forAllPar(gpy)(py => Par.equal(Par.map(py)(y => y), py))


  // Exercise 8.16 create richer generator for Par[Int] that build more deeply nested parallel computations
  /**
   * A `Gen[Par[Int]]` generated from list summation that spawns a new parallel computation for each element of the input list summed to
   * produce the final result
   *
   * This has to be a `lazy val` because of the way scala initialize objects. It depends on the `Prop` companion object being created references gpy2
   * */
  val gpy2: Gen[Par.Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(ys =>
    ys.foldLeft(Par.unit(0))((p, y) => Par.fork { Par.map2(p, Par.unit(y))(_ + _) })
  )

  def parTraverse[A, B](as: List[A])(f: A => Par.Par[B]): Par.Par[List[B]] = as.foldRight(Par.unit(Nil: List[B]))((a, pacc) => Par.fork(Par.map2(f(a), pacc)(_ :: _)))

  val gpy3: Gen[Par.Par[Int]] = Gen.choose(-100, 100).listOfN(Gen.choose(0, 20)).map(ys => Par.map(parTraverse(ys)(Par.unit))(_.sum))

  // Exercise 8.17: express that fork(x) == x
  val forkProp = Prop.forAllPar(gpy2)(y => Par.equal(Par.fork(y), y))

  // Exercise 8.18:
  def isEven(i: Int): Boolean = i % 2 == 0
  def takeWhileLoop: Prop = Prop.forAll(Gen.choose(-10, 10).list)(ys => ys.takeWhile(isEven).forall(isEven))

  // This one simply generate a constant and is not particularly interesting
  def genSTringIntFn(g: Gen[Int]): Gen[String => Int] = g.map(i => s => i)

}

// This is required to enable ** as a pattern
object ** {
  def unapply[A, B](p: (A, B)): Some[(A, B)]= Some(p)
}


case class CoGen[-A](run: (A, RNG) => RNG)
object CoGen {
  // Cons: when a generated function falsified, all we see is that for an opague function, the property fail
  def fn[A, B](in: CoGen[A], out: Gen[B]) : Gen[A => B] = Gen(State((rng: RNG) => {
    val (_, rng2) = rng.nextInt
    val f = (a: A) => out.sample.run(in.run(a, rng2))._1
    (f, rng2)
  }))

  def cogenInt: CoGen[Int] = CoGen((i, rng) => {
    val (seed, _) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)
  })

  def takeWhilePropInt = forAll(Gen.choose(-10, 10).list ** fn(cogenInt, Gen.boolean).unsized) { case (ys, f) => ys.takeWhile(f).forall(f) }

}

