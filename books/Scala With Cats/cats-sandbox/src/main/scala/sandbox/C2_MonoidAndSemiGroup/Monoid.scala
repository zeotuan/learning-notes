package sandbox.C2_MonoidAndSemiGroup

object Raw {
  trait SemiGroup[A] {
    def combine(a: A, b: A): A
  }

  trait Monoid[A] extends SemiGroup[A] {
    def empty: A
  }
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  def associativeLaw[A](a: A, b: A, c: A)(implicit m: Monoid[A]): Boolean = m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
  def identityLaw[A](a: A)(implicit m: Monoid[A]): Boolean = (m.combine(a, m.empty) == a) && (m.combine(m.empty, a) == a)

  object Monoid {
    implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      override def combine(a: Set[A], b: Set[A]): Set[A] = a union b
      override def empty: Set[A] = Set.empty
    }

    // intersection form a semigroup but not monoid
    implicit def setIntersectionSemigroup[A]: SemiGroup[Set[A]] = (a: Set[A], b: Set[A]) => a intersect b

    implicit def symDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
      override def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
      override def empty: Set[A] = Set.empty
    }
  }
}

object CatMonoid {
  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.semigroup._
  import cats.instances.map._

  Map("a" -> 1) |+| Map("b" -> 2)
  Option(1) |+| Option(2)
  ("hello", 123) |+| ("world", 321)

  def addAll[A](values: List[A])(implicit m: Monoid[A]): A = values.foldRight(m.empty)(_ |+| _)

  addAll(List(1, 2, 3))

  addAll(List(Some(1), Some(2), None))
}
