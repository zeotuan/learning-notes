package sandbox.C3_FunctorAndApplicativeFunctor

object Raw {
  // fa.map(a => a) == fa
  // fa.map(g(f(_))) == fa.map(f).map(g)
  trait Functor[F[_]] {
    def map[A, B](f: A => B): F[B]
  }
}

object CatFunctor {
  import cats.instances.list._ // for Functor
  import cats.instances.function._
  import cats.syntax.functor._
  import cats.Functor

  val func1: Int => Double = (x: Int) => x.toDouble
  val func2: Double => Double = (y: Double) => y * 2

  assert(
    (func1 map func2)(1) == 2 &&
      (func1 andThen func2)(1) == 2 &&
      func2(func1(1)) == 2
  )

  // function composition
  val func = ((x: Int) => x.toDouble)
    .map(x => x + 1)
    .map(x => x * 2)
    .map(x => s"$x!")

  func(123)



  /**
   * type constructor and higher kinded type
   * kind are like types for types. They describe the number of "holes" in a type
   * We distinguish between regular types that have no holes and “type constructors"
   * that have holes we can fill to produce types.
   *
   *
   * List - type constructor, takes one parameter
   * List[A] - type, produced by applying a type parameter
   *
   * Close analogy here with functions and values
   *
   * math.abs - function, takes one parameter
   * math.abs(x) - value, produced by applying a value paramete
   *
   *
   * */

  // simple functor for Option
  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    def map[A, B](value: Option[A])(func: A => B): Option[B] = value.map(func)
  }

  // functor can also sometime require additional parameter
  import scala.concurrent.{Future, ExecutionContext}
  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] = new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] = value.map(func)
  }


  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)

  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)

  val funcInt: Int => Int = (x: Int) => x + 1
  val listedFunc: Option[Int] => Option[Int] = Functor[Option].lift(funcInt)

  // write function that work on any functor context it's in
  // The F[_] type constructor allow us to insert any higher kinded type constructed with Int as parameter
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

  // Option functor - Option(22)
  doMath(Option(20))

  // List functor - List(3, 4, 5)
  doMath(List(1, 2, 3))

  /**
   * How does this work
   * within cats, a FunctorOps extension method is provided
   *
   * implicit class FunctorOps[F[_], A](src: F[A]) {
   *  def map[B](func: A => B)(implicit functor: Functor[F]): F[B] = functor.map(src)(func)
   * }
   *
   *  */

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A])  extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](value: Tree[A])(func: A => B): Tree[B] = value match {
      case Branch(left, right) => Branch(map(left)(func), map(right)(func))
      case Leaf(v) => Leaf(func(v))
    }
  }

  val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)) // cannot use map here due to invariant problem
  val tree2 = tree.map(_ * 2)
  Tree.branch(Branch(Leaf(1), Leaf(2)), Leaf(3)).map(_ * 2)

}