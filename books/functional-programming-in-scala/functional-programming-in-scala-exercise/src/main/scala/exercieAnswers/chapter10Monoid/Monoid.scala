package exercieAnswers.chapter10Monoid

import exercieAnswers.chapter08PropertyTesting._
import exercieAnswers.chapter07Parallelism.NonBlockingPar._

import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par.toParOps

/**
 * a monoid is a type together with a binary operation (combine) over that type, satisfying associativity and having an identity element (empty).
 * a monoid consists of the following:
 *
 * - Some type A
 *
 * - An associative binary operation, `combine` that takes two values of type A and combines them into one: `combine(combine(x,y),z) === combine(x,combine(y,z)) // for any choice of x: A, y: A, z:A`
 *
 * - A value, empty:A that is an identity function for that operation: `combine(x, empty) == combine(empty, x)` for any x: A
 * */
trait Monoid[A] {
  def combine(a1: A, a2: A): A //  Satisfies combine(combine(x, y), z) == combine(x, combine(y, z))
  def empty: A // Satisfies combine(x, empty) == x and combine(empty, x) == x
}

object Monoid {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def combine(a1: String, a2: String): String = a1 + a2
    def empty: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def combine(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    def empty: List[A] = Nil
  }

  // Exercise 10.1:
  val intAddition: Monoid[Int] = new Monoid[Int] {
    def combine(a1: Int, a2: Int): Int = a1 + a2
    def empty: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def combine(a1: Int, a2: Int): Int = a1 * a2
    def empty: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def empty: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def empty: Boolean = true
  }

  /**
   * Exercise 10.2:
   * this optionMonoid can be implemented in a different order which give different result.
   * Both still satisfy the monoid law but are not equivalent.
   * Every monoid have a __dual__ where `combine` operate in different order. However, Monoids
   * like `booleanOr` and `intAddition` are equivalent to their duals because their `combine` is
   * commutative and associative
   * */
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def combine(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def empty: Option[A] = None
  }

  /** Return __dual__ counterpart of any monoid */
  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def combine(a1: A, a2: A): A = m.combine(a2, a1)

    override def empty: A = m.empty
  }

  /**
   * A more flexible optionMonoid Implementation
   * f must be associative
   * */
  def optionMonoidF[A](f: (A, A) => A): Monoid[Option[A]] = new Monoid[Option[A]] {
    def combine(a1: Option[A], a2: Option[A]): Option[A] = for {
      aa1 <- a1
      aa2 <- a2
    } yield f(aa1, aa2)

    override def empty: Option[A] = None
  }

  def reverseBooleanOr: Monoid[Boolean] = dual(booleanOr)
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(optionMonoid)

  /**
   * Exercose 10.3: endofunction - a function having the same argument and return type
   * */
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def combine(a1: A => A, a2: A => A): A => A = a1 andThen a2
    def empty: A => A = a => a
  }
  /** endoMonoid but combine perform a1 compse a2 */
  def endoMonoid[A]: Monoid[A => A] = dual(endoMonoid)


  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    val associative = Prop.forAll(gen ** gen ** gen) { case x ** y ** z => m.combine(m.combine(x, y), z) == m.combine(x, m.combine(y, z))}
    val identity = Prop.forAll(gen) { a => m.combine(a, m.empty) == m.combine(m.empty, a)}
    associative && identity
  }

  /**
   * choosing foldRight also result in same result due to the law of associativity and identity
   * A left fold associates operations to the left
   * A right fold associates operation to the right
   * with the identity element on the left, right respectively
   * {{{
   *    words.foldLeft(stringMonoid.empty)(stringMonoid.combine)  == (("" + "Hic") + "Est") + "Index"
   *    words.foldRight(stringMonoid.empty)(stringMonoid.combine) == "Hic" + ("Est" + ("Index" + ""))
   * }}}
   *
   * */
  def combineAll[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.empty)(m.combine)

  /** Exercise 10.5: turn list to type that has supported monoid to apply combineAll */
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  /**
   * Exercise 10.6: foldLeft/Right via FoldMap
   * function type `(A, B) => B`, when curried is `A => (B => B)`
   * B => B is a monoid for any `B` (via function composition)
   * we need to get the dual version for endoMonoid since it's implement as  `f andThen g` instead of `f compose g`
   * */
  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = foldMap(as, dual(endoMonoid[B]))(f.curried)(z)

  /**
   * follow similar strategy but reverse the order of the parameters to f `(B,A) => B` instead of `(A, B) => B` compare to foldRight
   * keep the original order of function  composition
   * */
  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = foldMap(as, endoMonoid[B])(a => b => f(b, a))(z)

  /**
   * @param f - transform from A to B so monoid `m` can be applied
   * Since monoid's operation is associative, we can use balanced fold with a monoid which is efficient
   * for parallel computation:
   * combine(a, combine(b, combine(c, d))) == combine(combine(combine(a, b), c), d) == combine(combine(a,b), combine(c,d))
   *
   *
   * foldMap for IndexedSeq which split the sequence into two and recursively process each half and add
   * them together with the monoid
   * */
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = if (as.isEmpty) {
    m.empty
  } else if (as.length == 1) {
    f(as.head)
  } else {
    val (l,r) = as.splitAt(as.length/2)
    m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def combine(a1: Par[A], a2: Par[A]): Par[A] = a1.map2(a2)(m.combine)
    def empty: Par[A] = Par.unit(m.empty)
  }

  /**
   * Exercise 10.8: Implement parallel version of foldMap
   * First map over f over as using parMap so it run in parallel
   * then use foldMapV which supported balanced sequence with lifted monoid to parMonoid
   */
  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = Par.parMap(as)(f).flatMap(foldMapV(_, par(m))(Par.lazyUnit))

  /**
   * Exercise 10.9
   * This is a classical map-reduce problem
   * we can split the array in half and check if each half is ordered
   * additionally, we have make sure left half is smaller than right half (or greater)
   * to do this, we keep track of min and max value of each half
   * */
  def ordered(ints: IndexedSeq[Int]): Boolean = {
    case class Interval(ordered: Boolean, min: Int, max: Int)
    val orderedMonoid = new Monoid[Option[Interval]] {
      def combine(a1: Option[Interval], a2: Option[Interval]): Option[Interval] = (a1, a2) match {
        case (Some(aa1), Some(aa2)) => Some(Interval(aa1.ordered && aa2.ordered && aa1.max < aa2.min, aa1.min, aa2.max))
        case (x, None) => x
        case (None, x) => x
      }
      def empty: Option[Interval] = None
    }
    foldMapV(ints, orderedMonoid)(i => Some(Interval(ordered = true, i, i))).forall(_.ordered)
  }

  sealed trait WC
  /** case where we haven't seen any complete word yet*/
  case class Stub(chars: String) extends WC

  /**
   * @param lStub partial word we've seen on the left
   * @param words the number of complete words
   * @param rStub partial word we've seen on the right
   * */
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  // Exercise 10.10: Implement Monoid instance for WC
  val wcMonoid = new Monoid[WC] {

    def combine(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c1), Stub(c2)) => Stub(c1 + c2)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w , r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + w2 + (if ((r1 + l2).isEmpty) 0 else 1) , r2)
    }

    def empty: WC  = new Stub("")
  }

  def wcGen: Gen[WC] = {
    val smallString = Gen.choose(0, 10).flatMap(Gen.stringN)
    val genStub = smallString.map(Stub)
    val genPart = for {
      lStub <- smallString
      words <- Gen.choose(0, 10)
      rStub <- smallString
    } yield Part(lStub, words, rStub)
    Gen.union(genStub, genPart)
  }
  monoidLaws(wcMonoid, wcGen)

  /** use wc monoid to count words in a string by recursively splitting it into substring then count word within each substring */
  def count(s: String): Int = {
    def unstub(s: String): Int = s.length min 1
    foldMapV(s.toIndexedSeq, wcMonoid)(c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)) match {
      case Stub(cs) => unstub(cs)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  def foldMapG[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B = foldMap(as, m)(f)
  def foldMapVG[A, B](as: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B = foldMapV(as, m)(f)

  implicit val implicitIntMonoid: Monoid[Int] = intAddition

  val charCount = foldMapG(List("aabr", "dada", "adada"))(_.length)

  val allPositive = foldMapG(List(1,2,3))(_ > 0)(booleanOr)
}
