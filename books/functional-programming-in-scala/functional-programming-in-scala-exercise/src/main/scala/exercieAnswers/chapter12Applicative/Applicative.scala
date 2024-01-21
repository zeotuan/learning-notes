package exercieAnswers.chapter12Applicative

import exercieAnswers.chapter04ExceptionHandling.{Invalid, Valid, Validated}
import exercieAnswers.chapter09Parsers.SliceableTypes.Parser
import exercieAnswers.chapter10Monoid.{Monoid, Semigroup}
import exercieAnswers.chapter11Monad.Functor
import exercieAnswers.chapter12Applicative.Applicative.validatedApplicative

import java.sql.Date
import java.time.LocalDate

/**
 * Applicative computations have a fixed structure and simply sequence effects, whereas monadic computations may choose a structure dynamically based on result of previous effect
 * Applicative construct context-free computations, while monad allows for context sensitivity
 * Monad makes effects first class; they may be generated at interpretation time, rather than chosen ahead of time by the program
 *
 *
 * Advantages:
 * - In general, it's preferable to implement combinators like `traverse` using as few assumptions as possible. It's better to assume a data type can provide map2 than `flatMap`
 * - Applicative ís weaker than Monad, the interpreter of applicative effects has  more flexibility. Add flatMap is powerful, but it mean we're generating
 * our parsers dynamically, so the interpreter may be more limited in what it can do
 * - Applicative functor compose, whereas monads (in generals) do not.
 *
 *
 * Laws: Must obey functor laws:
 * - Left and Right identity:
 *   x.map(identity) == x
 *   x.map(f).map(g) == x.map(f andThen g)
 *  + map implementation must obey functor laws. in other words, map2 of some F[A] with unit preserve the structure of F[A].
 *  + unit(()).map2(fa)((_, a) => a)) == fa
 *  + fa.map2(unit(()))((a, _) => a)) == fa
 *
 *  - Associativity:
 *   combine(a, combine(b, c)) == combine(combine(a, b), c)
 *   compose(f, compose(g, h)) == compose(compose(f, g), h)
 *   fa.product(fb).product(fc) == fa.product(fb.product(fc)).map(assoc)
 *
 *  - Naturality of product:
 *    fa.map2(fb)((a, b) => (f(a), g(b))) == fa.map(f).product(fb.map(g))
 * */
trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa)(unit(()))((a, _) => f(a))
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List.empty[B]))((a, acc) => map2(f(a))(acc)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A])(fb: F[B]): F[(A, B)] = map2(fa)(fb)(_ -> _)

  def apply[A, B](fab: F[A => B], fa: F[A]): F[B] = map2(fa)(fab)((a, ab) => ab(a))

  def mapViaApply[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f), fa)
  /**
   * first currying the f so we get a function A => B => C, with unit we get F[A => B => C]
   * using apply with F[A], we now get F[B => C]
   * using apply again with F[B] and we now get F[C]
   * */
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = apply(apply(unit(f.curried), fa), fb)

  /** shorter version of above implementation since apply(unit(f.curried), fa) is actually map implementation */
  def map2WithMap[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = apply(map(fa)(f.curried), fb)
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(apply(map(fa)(f.curried), fb), fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply(apply(apply(map(fa)(f.curried), fb), fc), fd)

  /** Exercise 12.8: product of two applicative functors */
  def product[G[_]](g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), g.unit(a))

      override def apply[A, B](fab: (F[A => B], G[A => B]), fa: (F[A], G[A])): (F[B], G[B]) = (self.apply(fab._1, fa._1), g.apply(fab._2, fa._2))
    }
  }

  /** Exercise 12.9: Applicative functors also compose */
  def compose[G[_]](g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val self = this
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
      override def map2[A, B, C](fga: F[G[A]])(fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fga)(fgb)(g.map2(_)(_)(f))
    }
  }


}

object Applicative {
  import exercieAnswers.chapter04ExceptionHandling._
  type ZipList[+A] = LazyList[A]
  object ZipList {
    def fromLazyList[A](la: LazyList[A]): ZipList[A] = la
    def toLazyList[A](za: ZipList[A]): LazyList[A] = za

    def unit[A](a: => A): ZipList[A] = LazyList.continually(a)

    def map2[A, B, C](fa: ZipList[A])(fb: ZipList[B])(f: (A, B) => C): ZipList[C] = fa.zip(fb).map(f.tupled)

    /**
     * Exercise 12.4: sequence turns a finite list of potentially infinite lists into a potentially infinite list of finite lists.
     * The resulting zip list first emits a list of all the first elements from the inputs, then a list of all second elements,
     * and so on, until we reach the end of one of the input lists.
     * */
    def sequence[A](as: List[ZipList[A]]): ZipList[List[A]] = ???

    def assoc[A, B, C](p: (A, (B, C))): ((A, B), C) = p match {
      case (a, (b, c)) => ((a, b), c)
    }
  }


  def validatedApplicative[E]: Applicative[({type f[x] = Validated[E, x]})#f] = new Applicative[({type f[x] = Validated[E, x]})#f] {
    override def unit[A](a: => A): Validated[E, A] = Valid(a)

    /**
     * This trap implementation to use List[E] for Invalid
     * A cleaner implementation is to reimplement Invalid to take E
     *  */
    def map2[A, B, C](fa: Validated[E, A])(fb: Validated[E, B])(f: (A, B) => C)(implicit m: Monoid[List[E]]): Validated[E, C] = fa.map2(fb)(f)
  }
}

/**
 * Monad can now be made a subtype  of Applicative[F] by providing the default implementation of map2 in term of flatMap.
 * This tell that all monads are applicative functors
 * a minimal implementation of Monad must implement unit and override either flatMap or (join and map)
 *  */
trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))
  override def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a, b)))

  /**
   * {{{
   * def compose[G[_]](g: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
   *  val self = this
   *  new Monad[({type f[x] = F[G[x]]})#f] {
   *    override def unit[A](a: => A): F[G[A]] = self.unit(g.unit(a))
   *    // flatmap doesn't compile because g.map(ga)(a => f(a)) returns G[F[G[B]]]
   *    // however self.flatMap require F on the outside
   *    override def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = self.flatMap(fga)(ga =>g.map(ga)(f))
   *  }
   * }
   * }}}
   *  */

}

object Monad {
  /** Exercise 12.5: Write a monad for either */
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = fa.flatMap(f)
  }

  /**
   * Exercise 12.7: Prove that if somethings satisfy monad laws, it satisfy applicative law:
   * Need to show that the all the laws hold when implementing map2 in term of flatMap:
   *
   * - Left identity:
   * unit(()).map2(fa)((_, a) => a)          == fa
   * unit(()).flatMap(_ => fa.map(identity)) == fa
   * unit(()).flatMap(_ => fa)               == fa
   * compose(unit, _ => fa)(())              == fa // maybe not neccessary to use this
   * (_ => fa)(())                           == fa
   * fa                                      == fa
   *
   *
   * - Right identity:
   * fa.map2(unit(()))((a, _) => a)        == fa
   * fa.flatMap(a => unit(()).map(_ => a)) == fa
   * fa.flatMap(unit)                      == fa
   * fa                                    == fa
   *
   * - Associativity law:
   * fa.product(fb).product(fc) == fa.product(fb.product(fc)).map(assoc)
   * both can be reduced to this form
   * fa.flatMap(a =>
   *  fb.flatMap(b =>
   *    fc.flatMap(c => unit(((a, b), c)))))
   *
   * - Naturality laws: fa.map2(fb)((a, b) => (f(a), g(b))) == fa.map(f).product(fb.map(g))
   * left side reduction
   *  fa.flatMap(a => fb.map(b => (f(a), g(b))))
   * right side reduction
   *  fa.map(f)                  .map2(fb.map(g))(_ -> _)
   *  fa.map(f)                  .flatMap(x => fb.map(g).map(y => (x, y)))
   *  fa.map(f)                  .flatMap(x => fb.map(b => (x, g(b))))
   *  fa.flatMap(a => unit(f(a))).flatMap(x => fb.map(b => (x, g(b))))
   *  fa.flatMap(a => unit(f(a)).flatMap(x => fb.map(b => (x, g(b))))) // by monad left identity law, simplify unit(f(a)).flatMap(...).
   *  fa.flatMap(a => fb.map(b => f(a) -> f(b)))
   * */


}

object Example {
  case class Row(date: Date, temperature: Double)

  val F: Monad[Parser] = ???
  val d: Parser[Date] = ???
  val temp: Parser[Double] = ???
  val row: Parser[Row] = F.map2(d)(temp)(Row)
}


object WebFromExample {
  case class WebForm(name: String, birthdate: LocalDate, phoneNumber: String)

  def validName(name: String): Validated[String, String] = if (name != "") Valid(name) else Invalid(List("name cannot be empty"))
  def validBirthdate(birthdate: String): Validated[String, LocalDate] = try {
    Valid(LocalDate.parse(birthdate))
  } catch {
    case _: java.time.format.DateTimeParseException => Invalid(List("Birthdatemust be in form  yyyy-MM-dd"))
  }

  def validPhone(phoneNumber: String): Validated[String, String] = if (phoneNumber.matches("[0-9]{10}")) Valid(phoneNumber) else Invalid(List("Phone number must be 10 digits"))

  def validateWebForm(name: String, birthdate: String, phone: String): Validated[String, WebForm] = validatedApplicative.map3(
    validName(name),
    validBirthdate(birthdate),
    validPhone(phone)
  )(WebForm)
}

object WebFormNonEmptyListExample {
  case class NonEmptyList[+A](head: A, tail: List[A]) {
    def toList: List[A] = head :: tail
  }

  object NonEmptyList {
    def apply[A](head: A, tail: A*): NonEmptyList[A] = NonEmptyList(head, tail.toList)
  }

  def nelMonoid[A]: Monoid[NonEmptyList[A]] = new Monoid[NonEmptyList[A]] {
    override def empty: NonEmptyList[A] = ???
    override def combine(a1: NonEmptyList[A], a2: NonEmptyList[A]): NonEmptyList[A] = NonEmptyList(a1.head, a1.tail ++ a2.toList)
  }

  def nelSemiGroup[A]: Semigroup[NonEmptyList[A]] = (a1: NonEmptyList[A], a2: NonEmptyList[A]) => NonEmptyList(a1.head, a1.tail ++ a2.toList)

}
