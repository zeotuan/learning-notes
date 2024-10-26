package sandbox.C6_SemiGroupal_Applicative

import cats.Semigroup
import cats.implicits.catsSyntaxSemigroup

/**
 * flatMap and map are useful but there are some flows that they cannot express
 * For example:
 * - form validation where we want to accumulate all errors instead of early termination
 * - parallel processing where we want to run multiple tasks and combine the results since
 *   flatMap is sequential where each step depends on the previous one
 *
 * Some other type classes that can help us with these limitations are:
 * - SemiGroupal: Encompasses the idea of combining independent contexts
 * - Parallel: convert a type with a Monad to a related type with a SemiGroupal instance
 * - Applicative extends Semigroupal and Functor and the source of pure method
 * */
object flatMapLimitations {
  import cats.syntax.either._
  def parseInt(str: String): Either[String, Int] = {
    Either.catchOnly[NumberFormatException](str.toInt).leftMap(_ => s"Couldn't read $str")
  }

  for {
    a <- parseInt("1")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield a + b + c // Either[String, Int] = Left("Couldn't read b")
}

/**
 * A type that allow combining of independent contexts
 * */
trait Semigroupal[F[_]] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
}

/**
 * SemiGroupal law: product must be associative:
 * product(a, product(b, c)) == product(product(a, b), c)
 * */
object SemiGroupal {
  import cats.Semigroupal
  import cats.instances.option._

  Semigroupal[Option].product(Some(123), Some("abc")) // Some((123, "abc"))
  Semigroupal[Option].product(None, Some("abc")) // None
  Semigroupal[Option].product(Some(123), None) // None

  Semigroupal.tuple3(Option(1),Option(1),Option(2)) // Some(1,2,3)
  Semigroupal.tuple3(Some(1),Some(1),None) // None

  Semigroupal.map3(Option(1),Option(1),Option(2))(_ + _ + _) // Some(4)
  Semigroupal.map3(Option(1),Option(1), None)(_ + _ + _) // None

  import cats.syntax.apply._
  (Option(123), Option("abc")).tupled // Some((123, "abc"))
  (Option("abcd"), Option(123), Option("abc")).tupled // Some(("abcd", 123, "abc))

  final case class Cat(name: String, born: Int, color: String)
  val cat1: Option[Cat] = (Option("abcd"), Option(123), Option("abc")).mapN(Cat.apply) // Some(Cat("Garfield", 1978, "Orange & black"))
  // Internally mapN uses Semigroupal to combine the Option values and then applies the function to the result

  import cats.Monoid
  import cats.instances.int._ // for Monoid
  import cats.instances.invariant._ // for Semigroupal
  import cats.instances.list._ // for Monoid
  import cats.instances.string._ // for Monoid
  import cats.syntax.apply._ // for imapN

  // imapN and contramapN which accept Contravariant and Invariant functors respectively
  final case class Cat2(name: String,
                       yearOfBirth: Int,
                       favoriteFoods: List[String]
                      )

  val tupleToCat: (String, Int, List[String]) => Cat2 =
    Cat2.apply _
  val catToTuple: Cat2 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
  implicit val catMonoid: Monoid[Cat2] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  val garfield = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk Food"))
  garfield |+| heathcliff

  // res14: Cat2 = Cat2("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food"))

  /** ========================== SemiGroupal Apply to different type ========================== */
  object applyFuture {
    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

    // Future execute eagerly even before we call Await.result
    Await.result(futurePair, 1.second) // res15: (String, Int) = ("Hello", 123)

    val futureCats: Future[Cat2] = (
      Future("Garfield"),
      Future(1978),
      Future(List("Lasagne"))
    ).mapN(Cat2.apply)

    Await.result(futureCats, 1.second) // res16: Cat2 = Cat2("Garfield", 1978, List("Lasagne"))
  }

  // Combining list using SemiGroupal produce a cartesian product instead of zipping
  // This is similar to product in List Monad
  object applyList {
    import cats.instances.list._
    Semigroupal[List].product(List(1, 2), List(3, 4)) // List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  }

  // Similar to flatMap, SemiGroupal also fail fast when one of the context "falsy"
  // even when it's possible to inspect the other context
  object applyEither {
    import cats.instances.either._
    type ErrorOr[A] = Either[Seq[String], A]
    Semigroupal[ErrorOr].product(
      Left(Seq("Error 1")),
      Left(Seq("Error 2"))
    ) // ErrorOr[(Nothing, Nothing)] Left(List("Error 1"))
  }

  /**
   * The reason the behaviour for List and Either is supprising is because they are both Monad
   * This can also be seen in the implementation of Semigroupal for other monad like Option
   * Since monad instance for these type are readily available, the default product implementation
   * is provided. remember Monad is a Semigroupal but not vice versa
   *
   * The Future trick is possible since Future is created before composing them using Semigroupal
   *
   * The real parrallel happen when we provide an instance of Semigroupal (and Applicative) for a type but is  not a Monad
   *
   *
   *  */
   object productSample {

    import cats.Monad
    import cats.syntax.functor._ // for map
    import cats.syntax.flatMap._ // for flatmap

    // This explain cartesian product behaviour of Semigroupal for List
    def product[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
  }

}

