package exercieAnswers.chapter10Monoid

import exercieAnswers.chapter08PropertyTesting._

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
  /** endoMonoid but combine perform a1 compose a2 */
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

}
