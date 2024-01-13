package exercieAnswers.chapter11Monad

import exercieAnswers.chapter06State.State
import exercieAnswers.chapter08PropertyTesting._
import exercieAnswers.chapter07Parallelism.NonBlockingPar._
import exercieAnswers.chapter07Parallelism.NonBlockingPar.Par.toParOps
import exercieAnswers.chapter09Parsers.Parsers
import exercieAnswers.chapter09Parsers.SliceableTypes.Parser
import exercieAnswers.chapter11Monad.Monad.stateMonad

import scala.annotation.tailrec

/**
 * a functor is an impplementation of map that preserve the structure of the data type
 * the functor law are:
 * - identity: x.map(identity) == x
 * - composition: x.map(f).map(g) == x.map(f andThen g)
 * */
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) = (map(fab)(_._1), map(fab)(_._2))
  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(a) => map(a)(Left(_))
    case Right(b) => map(b)(Right(_))
  }
}

object Functor {
  val listFunction = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
}

/**
 * A monad is an implementation of one of the minimal sets of monadic combinators,
 * satisfying the laws of associativity and identity
 *
 * associative law: compose(compose(f,g),h) == compose(f, compose(g,h))
 * identity law: compose(f, unit) == compose(unit, f) == f
 *
 * a Monad must implement one of these 3 set of primitive monad combinators:
 * - unit and flatMap
 * - unit and compose
 * - unit, map, and join
 *
 * monad provide a context for introducing and binding variables and performing variable substitution
 *
 * The Monad contract doesn’t specify what is happening between the lines, only that whatever is happening
 * satisfies the laws of associativity and identity.
 *
 * Providing a Monad instance for a type constructor has practical usefulness. Doing so gives access to
 * all of the derived operations (or combinators) in exchange for implementing one of the minimal sets
 * of monadic combinators.
 * */
trait Monad[F[_]] extends Functor[F] {
  /** since flatMap is implemented in term of join and vice versa, one of them need to be overridden to avoid infinite loop */
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def unit[A](a: => A): F[A]
  def map2[A, B, C](fa: F[A])(fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))



  // Exercise 11.3 Implement other common function
  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(List.empty[B]))((a, b) => map2(f(a))(b)(_ :: _))

  // traverse can also be implemented via sequence
  // But it can cause infinite loop
  // therefore one of them must be reimplemented other way
  def traverseViaSequence[A, B](as: List[A])(f: A => F[B]): F[List[B]] = sequence(as.map(f))

  /** repeats the supplied nomadic value n times, combining the results into a single value,
   * where the nomadic type defines how that combinations is performed */
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A, B](fa: F[A])(fb: F[B]): F[(A, B)] = map2(fa)(fb)(_ -> _)

  /** Exercise 11.6: */
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] = as.foldRight(unit(List.empty[A]))((a, acc) => flatMap(f(a))(b => if (b) map2(unit(a))(acc)(_ :: _) else acc))

  /**
   * Exercise 11.7: Implement the Kleisli composition function compose
   * This allow associative law for monads to be stated in a more symmetric ways
   * compose(compose(f,g),h) == compose(f, compose(g,h))
   * */
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))


  /**
   * Exercise 11.8: Implement flatMap via Compose
   * */
  def flatMapViaCompose[A, B](fa: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => fa, f)(())

  /**
   * Exercise 11.9:
   * compose(f, compose(g,h)) == compose(compose(f,g),h)
   *
   * a => f(a).flatMap(compose(g,h)) == a => compose(f, g)(a).flatMap(h)
   *
   * a => f(a).flatMap(b => g(b).flatMap(h)) == a => (b => f(b).flatMap(g))(a).flatMap(h)
   *
   * a => f(a).flatMap(g).flatMap(h) == a => f(a).flatMap(g).flatMap(h)
   * */


  /**
   * Identity laws
   * compose(f, unit) == compose(unit, f) == f
   *
   * Exercise 11.10: Prove
   * x.flatMap(unit) == x
   * unit(y).flatMap(f) = f(y)
   *
   * 1- Prove compose(f, unit) == f is equivalent to  x.flatMap(unit) == x
   *
   * compose(f, unit)        == f
   * a => f(a).flatMap(unit) == a => f(a)
   * x.flatMap(unit)         == x // replace a => f(a) with x
   *
   *
   *
   * 2 - Prove compose(unit, x) == f is equivalent to unit(y).flatMap(f) == f(y)
   * compose(unit, f) == f
   * a => unit(a).flatMap(f) == f
   * (a => unit(a).flatMap(f))(y) == f(y) // apply y to both side
   * unit(y).flatMap(f) == f(y) // substitute a with y
   * */

  /**
   * Exercise 11:12:
   * compose(f, unit) == f is equivalent to x.flatMap(unit) == x
   *
   * when x is None
   * None.flatMap(unit) == None
   * None == None // None.flatMap(anyF) is None
   *
   * when x is Some
   * Some(a).flatMap(unit) == Some(a) // substitute x with Some(a)
   * Some(a) == Some(a) // left hand side is reduce to unit(a) which reduce to Some(a)
   *
   *
   * compose(unit, f) == f is equivalent to unit(y).flatMap(f) == f(y)
   * unit(y).flatMap(f) == f(y)
   * Some(y).flatMap(f) == f(y) // unit(y) reduce to Some(y)
   * f(y) == f(y) // simplify flatMap
   * */

  /** Exercise 11.13: Third nomadic operator join or sometime called flatten */
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)
  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => join(map(f(a))(g))


  /**
   * Exercise 11.14: reinstate the law in term of map and join
   * x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
   * x.map(f).join.map(g).join == x.map(a => f(a).map(g).join).join
   * x.join.map(g).join == x.map(a => a.map(g).join).join // choose f as identity
   * x.join.join == x.map(_.join).join // chose g as identity
   *
   * rewrite right identity law
   * x.flatMap(unit)  == x
   * x.map(unit).join == x
   *
   * rewrite left identity law
   * unit(y).flatMap(f) == f(y)
   * unit(y).map(f).join == f(y)
   * unit(y).join == y
   * */

  /**
   * Exercise 11.15:
   * Let’s use the join version of the associative law:
   * x.join.join == x.map(_.join).join.
   * Note that x has the `F[F[F[A]]]` type here.
   *
   * For Par, the associative law says that if you have a three-level deep parallel computation,
   * you can await the results of the computations inside out (left side) or outside in (right side), and the result is equivalent.
   *
   * For Parser, the associative law says that in a series of dependent parsers, only the order of the parsers matters,
   * not the way in which they are nested.
   * */

  /**
   * Exercise 11.16:
   *
   *
   * Right identity law — x.flatMap (unit) == x
   * Left identity law — unit(y).flatMap(f) == f(y)
   * For Gen, the right identity law means that flat mapping the unit generator over a generator does not change it in any way.
   * The left identity law means that the generator you get from lifting an arbitrary value via the unit generator and then flat
   * mapping that over an arbitrary function is equivalent to simply applying that value directly to the function. Travelling
   * through the unit generator and flatMap has no effect on the generated values.
   *
   *
   * For List, the right identity law means that flat mapping a singleton list constructor over each element results in the original
   * list—that flatMap can’t drop, filter, or otherwise change the elements. The left identity law means that flatMap can’t increase
   * or decrease the number of elements (e.g., by applying the supplied function multiple times to each element before joining the results).
   *
   * */
}

case class Reader[R, A](run: R => A)
object Monad {
  // Exercise 11.1: create monad Instance for Option, List, LazyList, Par, Parser
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa.flatMap(f)
  }

  val optionaMonad = new Monad[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
  }

  val listMonad = new Monad[List] {
    def unit[A](a: => A): List[A] = List[A](a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  val lazyListMonad = new Monad[LazyList] {
    def unit[A](a: => A): LazyList[A] = LazyList[A](a)
    override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] = fa.flatMap(f)
  }

  val parMonad = new Monad[Par] {
    def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](fa: Par[A])(f: A => Par[B]): Par[B] = fa.flatMap(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A, B](fa: P[A])(f: A => P[B]): P[B] = p.flatMap(fa)(f)
  }

  // Exercise 11.2
  type IntState[A] = State[Int, A]
  val intStateMonad = new Monad[IntState] {
    def unit[A](a: => A): IntState[A] = State(s => (a, s))
    override def flatMap[A, B](fa: IntState[A])(f: A => IntState[B]): IntState[B] = fa.flatMap(f)
  }

  case class Id[A](value: A) {
    def map[B](f: A => B): Id[B] = Id(f(value))
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: => A): Id[A] = Id(a)

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = fa.flatMap(f)
  }

  def stateMonad[S]: Monad[({type f[x] = State[S, x]})#f] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)
  }

  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(fa.run(r)).run(r))
  }
}

object Reader {
  def ask[R]: Reader[R, R] = Reader(r => r)
  def apply[R, A](f: R => A): Reader[R, A] = Reader(f)
}

object Example {
  case class Item(name: String, price: Double)
  case class Order(item: Item, quantity: Int)

  val genOrder: Gen[Order] = for {
    name <- Gen.stringN(3)
    price <- Gen.choose(10, 50).map(_ * 10)
    quantity <- Gen.choose(1, 100)
  } yield Order(Item(name, price), quantity)

  val genItem: Gen[Item] = for {
    name <- Gen.stringN(3)
    price <- Gen.choose(10, 50).map(_ * 10)
  } yield Item(name, price)

  val genOrder2: Gen[Order] = for {
    item <- genItem
    quantity <- Gen.choose(1, 100)
  } yield Order(item, quantity)

  // genOrder and genOrder2 are the same because flatMap obeys associative laws
  // x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))

  /**
   * Exercise 11.18: What is the meaning of replicatedM and sequence for stateMonad
   *
   * replicateM writes up all the state actions one after another. The output of the
   * first state action is the input state to the second actions and so on and collects
   * the output value from each action into a list
   *
   * map2 and sequence also evaluate state actions sequentially, passing output of an action
   * as input state to the next action in the sequence. map2 allows the results values of
   * the two state actions to be combined into a new value via arbitrary function. while sequence
   * collects the various output value into output
   *
   * */
  val getAndIncrement: State[Int, Int] = for {
    i <- State.get
    _ <- State.set(i + 1)
  } yield i

  stateMonad[Int].replicateM(10, getAndIncrement).run(0) // (List[Int], Int) = (List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),10)

  stateMonad[Int].map2(getAndIncrement)(getAndIncrement)(_ -> _).run(0) // ((Int, Int), Int) = ((0,1),2)

  stateMonad[Int].sequence(List.fill(3)(getAndIncrement)).run(0) // (List[Int], Int) = (List(0, 1, 2), 3)


  /**
   * Exercise 11.19: what laws are expected to mutually hold for get, set, unit and flatMap for stateMonad
   *
   * - get followed by set must be equivalent to unit
   * State.get.flatMap(s => State.set(s)) == State.unit(())
   *
   * - setting the state to some value then getting it must be equivalent to a unit of that value
   * s => (State.set(s).flatMap(_ => State.get)) == s => State.unit(s)
   *
   * what does this tell about State monad ?
   *
   * At each line in the for-comprehension, the implementation of flatMap is making sure that the current state is
   * available to get and the new state gets propagated to all actions that follow a set.
   * (For clarity Check State flatMap implementation)
   *
   * The monad cotract doesn't specify what is happening between the lines, only that whatever is happening satisfies
   * the law of associativity and identity
   *
   * */
  val F = stateMonad[Int]

  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List.empty[(Int, A)]))((acc,a) => for {
      xs <- acc
      n  <- State.get
      _  <- State.set(n + 1)
    } yield (n, a) :: xs).run(0)._1.reverse

  def zipWithIndex[A](as: List[A]): List[(Int,A)] =
    as.foldLeft(F.unit(List.empty[(Int, A)]))((acc, a) =>
      acc.flatMap(xs =>
        State.get.flatMap(n =>
          State.set(n + 1).map(_ =>
            (n, a) :: xs
          )
        )
      )
    ).run(0)._1.reverse
}
