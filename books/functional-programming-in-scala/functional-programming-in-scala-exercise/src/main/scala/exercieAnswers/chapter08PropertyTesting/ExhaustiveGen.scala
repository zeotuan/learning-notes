package exercieAnswers.chapter08PropertyTesting

import exercieAnswers.chapter05Laziness._
import exercieAnswers.chapter06State.{RNG, State}
import exercieAnswers.chapter08PropertyTesting.Prop.{Falsified, MaxSize, Passed, Result, TestCases, forAll, randomLazyList}
import exercieAnswers.chapter07Parallelism.Par
import exercieAnswers.chapter08PropertyTesting.Gen.{map2LazyList, map2Option, unbounded}

import java.util.concurrent.Executors
import scala.util.control.NonFatal

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  self =>

  // Exercise 8.9 Implement && and || operator
  def &&(that: Prop): Prop = Prop {
    (max, n, rng) => tag("and-left").run(max, n, rng) match {
      case Passed(_, an) => that.tag("and-right").run(max, an, rng) match {
        case Passed(s, sn) => Passed(s, an + sn)
        case x => x
      }
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

  sealed trait Status
  case object Proven extends Status
  case object Unfalsified extends Status

  sealed trait Result

  case class Passed(status: Status, testCases: TestCases) extends Result
  case class Falsified(failure: FailedCase, success: SuccessCount) extends Result

  def apply(f: (TestCases, RNG) => Result): Prop = Prop((_, n, rng) => f(n, rng))

  /** produce an infinite random lazy list from `Gen` and a starting rng*/
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] = LazyList.unfold(rng)(r => Some(g.sample.run(r))) // generate infinite lazy list of random value by sampling the generator

  def buildMsg[A](s: A, e: Throwable): String = s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, _,rng) => {
      def go(i: Int, j: Int, l: LazyList[Option[A]], onEnd: Int => Result) : Result= if (i == j) {
        Passed(Unfalsified, i)
      } else l match {
        case Cons(h, t)  => h() match {
          case Some(h) => try {
            if (f(h)) { go(i+1, j, t(), onEnd) }
            else Falsified(h.toString, i)
          } catch {
            case NonFatal(e) => Falsified(buildMsg(h, e), i)
          }
        }
        case None => Passed(Unfalsified, i)
        case _ => onEnd(i)
      }
     go(0, n/3, as.exhaustive, i => Passed(Proven, i)) match {
       case Passed(Unfalsified, _) =>
         val rands = randomLazyList(as)(rng).map(Some(_))
         go(n/3, n, rands, i => Passed(Unfalsified,  i))
       case s => s
     }
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSiez = n / (max + 1)
      val props: List[Prop] = LazyList.from(0).take(max + 1).map(i => forAll(g(i))(f)).toList
      val p: Prop = props.map(p => Prop { (max, n, rng) => p.run(max, n, rng)}).reduceLeft(_ && _)
      p.run(max, n, rng) match {
        case Passed(Proven, n) => Passed(Unfalsified, n)
        case x => x
      }
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = g match {
    case Unsized(g1) => forAll(g1)(f)
    case Sized(gs) => forAll(gs)(f)
  }

  def run(p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit= {
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests: \n $msg.")
      case Passed(Unfalsified, n) => println(s"+ OK, property unfalsified, ran $n tests.")
      case Passed(Proven, n) => println(s"+ OK, property proven, ran $n tests.")
    }
  }

  def verify(p: => Boolean): Prop = Prop { (_, _, _) => if (p)  Passed(Proven, 1) else Falsified("()", 0)}

  val executors = Gen.weighted(Gen.choose(1,4).map(Executors.newFixedThreadPool) -> 0.75, Gen.unit(Executors.newCachedThreadPool) -> 0.25)
  def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors.map2(g)(_ -> _)) { case (s, a) => f(a)(s).get }

  def forAllPar1[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors ** g) { case (s, a) => f(a)(s).get }

  def forAllPar2[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop = forAll(executors ** g) { case s ** a => f(a)(s).get }

  def verifyPar(p: Par.Par[Boolean]): Prop = forAllPar(Gen.unit(()))(_ => p)
}


case class Gen[+A](sample: State[RNG, A], exhaustive: LazyList[Option[A]]) {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???


  // Exercise 8.6: Implement flatMap and a more dynamic listOfN version using it
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample), exhaustive.flatMap {
    case None => LazyList(None)
    case Some(a) => f(a).exhaustive
  })

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f), exhaustive.map(_.map(f)))

  def map2[B, C](g: Gen[B])(f: (A,B) => C): Gen[C] = Gen(sample.map2(g.sample)(f), map2LazyList(exhaustive, g.exhaustive)(map2Option(_, _)(f)))

  /* A method alias for the function we wrote earlier. */
  def listOfN(size: Int): Gen[List[A]] = Gen.listOfN(size, this)

  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => listOfN(s))

  def unsized = SGen(_ => this)

  def list: SGen[List[A]] = Gen.listOf(this)
  def nonEmptyList: SGen[List[A]] =  Gen.listOf(this, 1)

  def **[B](gb: Gen[B]): Gen[(A,B)] = map2(gb)(_ -> _)

}

object Gen {

  type Domain[+A] = LazyList[Option[A]]
  def bounded[A](a: LazyList[A]): Domain[A] = a.map(Some(_))
  def unbounded: Domain[Nothing] = LazyList(None)

  /**
   * Exercise 8.4: choose generate a random integer in range [start, stopExclusive)
   * */
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(
    State(RNG.nonNegative).map(n => start + n % (stopExclusive - start)),
    bounded(LazyList.from(start).take(stopExclusive - start))
  )

  // Exercise 8.5
  def unit[A](a: A): Gen[A] = Gen(State.unit(a), bounded(LazyList(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean), bounded(LazyList(true, false)))
  def double: Gen[Double] = Gen(State(RNG.double), unbounded)
  def int: Gen[Int] = Gen(State(RNG.int), unbounded)

  /**
   * Generates all possible combinations of a `LazyList[LazyList[A]]`. For instance:
   * cartesian(LazyList(LazyList(1,2), LazyList(3), LazyList(4,5))) ==
   * LazyList(LazyList(1,3,4), LazyList(1,3,5), LazyList(2,3,4), LazyList(2,3,5))
   * */
  def cartesian[A](s: LazyList[LazyList[A]]): LazyList[LazyList[A]] = s.foldRight(LazyList(LazyList[A]())) { (hs, ts) => map2LazyList(hs,ts)(LazyList.cons(_, _)) }

  def sequenceOption[A](o: List[Option[A]]): Option[List[A]] = o.foldLeft[Option[List[A]]](Some(List())) {  (t, h) => map2Option(h, t)(_ :: _).map(_.reverse) }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(
    State.sequence(List.fill(n)(g.sample)),
    cartesian(LazyList.continually(g.exhaustive).take(n)).map(l => sequenceOption(l.toList))
  )

  // Exercise 8.7: Implement union which combine two Gen of same type into 1 by pulling value from each generator based on equal likelyhood

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(State(RNG.boolean).flatMap(b => if (b) g1.sample else g2.sample), interleave(g1.exhaustive, g2.exhaustive))
  def union1[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)

  /** Interleave two lazyList element into single list */
  def interleave[A](s1: LazyList[A], s2: LazyList[A]): LazyList[A] = s1.zipAll(s2).flatMap { case (a, a2) => LazyList((a.toList ++ a2.toList): _*)}


  /** Interleave two lazyList element into single list Same as
   * above but use b to control which lazyList to pull element from at each step
   * if one list is exhausted, append all element from the other list
   * */
  def interleave[A](b: LazyList[Boolean], s1: LazyList[A], s2: LazyList[A]): LazyList[A] = b.headOption.map { hd =>
    if(hd) s1 match {
      case Cons(h, t) => LazyList.cons(h(), interleave(b.drop(1), t(), s2))
    } else s2 match {
      case Cons(h, t) => LazyList.cons(h(), interleave(b.drop(1), s1, t()))
    }
  } getOrElse LazyList.empty

  // Exercise 8.8: a weighted version of union
  // generate a probability in range [0,1)
  // select g1 if it is less than normalized weighting
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs) // this is the probability of choosing g1 over g2
    def bools: LazyList[Boolean] = randomLazyList(double.map(_ < threshold))(RNG.Simple(302837L))
    Gen(State(RNG.double).flatMap(d => if (d < threshold) g1._1.sample else g2._1.sample), interleave(bools, g1._1.exhaustive, g2._1.exhaustive))
  }

  def listOf[A](g: Gen[A], min: Int = 0): SGen[List[A]] = SGen(n => g.listOfN(n.max(min)))

  def map2LazyList[A, B, C](sa: LazyList[A], sb: => LazyList[B])(f:  (A,  => B) => C): LazyList[C] = for {
    a <- sa
    b <- sb
  } yield f(a,b)

  def map2Option[A, B, C](sa: Option[A], sb: Option[B])(f: (A, B) => C): Option[C] = for {
    a <- sa
    b <- sb
  } yield f(a,b)

  def even(start: Int, stopExclusive: Int): Gen[Int] = choose(start, if (stopExclusive%2 == 0) stopExclusive-1 else stopExclusive).map(n => if (n%2 == 0) n else n+1)
  def odd(start: Int, stopExclusive: Int): Gen[Int] = choose(start, if (stopExclusive%2 == 0) stopExclusive else stopExclusive-1).map(n => if (n%2 == 0) n+1 else n)

  //TODO: Fix For comprehension does not work for Gen ???
  def sameParity(from: Int, to: Int): Gen[(Int, Int)] = choose(from, to).flatMap { i =>
    (if (i%2 ==0) even(from, to) else odd(from, to)).map(j => (i, j))
  }

  def genStringfn[A](g: Gen[A]): Gen[String => A] = Gen(State((rng: RNG) => {
    val (seed, rng2) = rng.nextInt // use rng to produce seed so we get a new function each time
    // use hashcode of the string to perturb the rng state. This make sure that any unique
    // String value will produce a unique function
    val f = (s: String) => g.sample.run(RNG.Simple(seed.toLong ^ s.hashCode.toLong))._1
    (f, rng2)
  }), unbounded)

}


sealed trait SGen[+A] {

  def map[B](f: A => B): SGen[B] = this match {
    case Sized(g) => Sized(g.andThen(_.map(f)))
    case Unsized(g) => Unsized(g.map(f))
  }

  def flatMap[B](f: A => Gen[B]): SGen[B] = this match {
    case Sized(g) => Sized(g.andThen(_.flatMap(f)))
    case Unsized(g) => Unsized(g.flatMap(f))
  }

  def **[B](sg: SGen[B]): SGen[(A, B)] =(this, sg) match {
    case (Sized(g1), Sized(g2)) => Sized(n => g1(n) ** g2(n))
    case (Unsized(g1),Unsized(g2)) => Unsized(g1 ** g2)
    case (Sized(g1), Unsized(g2)) => Sized(n =>g1(n) ** g2)
    case (Unsized(g1), Sized(g2)) => Sized(n =>g1 ** g2(n))
  }
}

case class Sized[A](forSize: Int => Gen[A]) extends SGen[A]
case class Unsized[A](get: Gen[A]) extends SGen[A]


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
  }), unbounded)

  def cogenInt: CoGen[Int] = CoGen((i, rng) => {
    val (seed, _) = rng.nextInt
    RNG.Simple(seed.toLong ^ i.toLong)
  })

  def takeWhilePropInt = forAll(Gen.choose(-10, 10).list ** fn(cogenInt, Gen.boolean).unsized) { case (ys, f) => ys.takeWhile(f).forall(f) }

}