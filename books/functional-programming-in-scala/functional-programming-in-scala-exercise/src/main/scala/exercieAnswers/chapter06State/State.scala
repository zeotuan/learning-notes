package exercieAnswers.chapter06State

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }


}

object RNG {
  case class Simple(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed) // next RNG state to be used
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }


  // using the same Simple to create pseudo random number
  // will always generate the same number
  // to create new random number using Simple
  val rng1 = new Simple(42)
  val (randomN1, rng2) = rng1.nextInt
  val (randomN2, rng3) = rng2.nextInt
  val (randomN3, rng4) = rng3.nextInt
  // and so on

  // Exercise 6.1
  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, r1) = rng.nextInt
    (if (n < 0) -(n + 1) else n, r1)
  }

  // Exercise 6.2: function that generate random double in range 0 to 1
  def double(rng: RNG): (Double, RNG) = {
    val (i, r1) = nonNegative(rng)
    (i / (Int.MaxValue.toDouble + 1), r1)
  }

  // Exercise 6.3 Implement intDouble, doubleInt, double3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1 ,d2, d3), r3)
  }


  // Exercise 6.4: Implement a function  that generate a random list of integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(c: Int, acc: List[Int], rng: RNG): (List[Int], RNG) = if (c > 0) {
      val (i, r) = rng.nextInt
      go(c - 1, i :: acc, r)
    } else {
      (acc, rng)
    }
    go(count, Nil, rng)
  }

  // function follow this form RNG => (A, RNG) are called state actions or state transition
  // since we will be repeat this form alots, it's good practice to define a type alias for it
  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = (a, _)

  def  map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, r) = s(rng)
    (f(a), r)
  }


  // Exercise 6.5: ImplementDouble Using map
  def nonNegativeEven: Rand[Int] = map(nonNegative)(i => i - (i%2))
  def doubleViaMap: Rand[Double] = map(nonNegative)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6.6: Implement map2
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rngA) = ra(rng)
    val (b, rngB) = rb(rngA)
    (f(a,b), rngB)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)


  // Exercise 6.7: Implement sequence
  // This function is very similar to sequence we implement for Option type
  // since RNG is never mentioned, we could create a polymorphic version of it
  // we can also use foldLeft but the result list would be in reversed order
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] = rs.foldRight(unit(Nil: List[A]))((a, acc) => map2(a, acc)(_ :: _))


  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val (i, rng2) = nonNegative(rng)
    // using just the most would satisfy the condition
    // however it would skew
    // so we retry to get another random integer that is divisible to n
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (mod, rng2)
    else
      nonNegativeLessThan(n)(rng2)
  }

  // Exercise 6.8: Implement flatmap then  implement nonNegativeLessthan
  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = r(rng)
    f(a)(rng1)
  }

  // Exercise 6.9: Implement map, map2, nonNegativeThan via flatMap
  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] = flatMap(nonNegative)(i => {
    val mod = i%n
    if (i + (n - 1) - mod >= 0)
     unit(mod)
    else
      nonNegativeLessThanViaFlatMap(n)
  })

  def  mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

  def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + 1)
}

import State._
case class State[S, +A](run: S => (A, S)) {
  // Exercise 6.10: Implement unit, map, map2, flatMap, sequence

 def  map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    // call the underlying state to obtain new s1 and a
    // apply f and run result state to obtain tuple
    val (a, s1) = run(s)
    f(a).run(s1)
  })

}

object State {
  // type Rand[A] = State[RNG, A]
  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  // Exercise 6.10: Implement Sequence
  def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] = rs.foldRight(unit[S, List[A]](Nil))((a, acc) => a.map2(acc)(_ :: _))
  def sequenceViaFoldLeft[S, A](rs: List[State[S, A]]): State[S, List[A]] = rs.reverse.foldLeft(unit[S, List[A]](Nil))((acc, a) => a.map2(acc)(_ :: _))

  def traverse[S, A, B](as: List[A])(f: A => State[S, B]): State[S, List[B]] = as.foldRight(unit[S, List[B]](Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  // This is similar to foldLeft
  def sequenceViaLoop[S, A](rs: List[State[S, A]]): State[S, List[A]] = {
    @tailrec
    def go(s: S, action: List[State[S, A]], acc: List[A]): (List[A], S) = action match {
      case Nil => (acc.reverse, s)
      case h::t => {
        val (a, s2) = h.run(s)
        go(s2, t, a :: acc)
      }
    }
    State(s => go(s, rs, Nil))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}