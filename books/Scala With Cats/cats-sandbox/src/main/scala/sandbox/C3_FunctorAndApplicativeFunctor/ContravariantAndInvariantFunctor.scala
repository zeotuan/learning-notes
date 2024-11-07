package sandbox.C3_FunctorAndApplicativeFunctor

import cats.Invariant.ops.toAllInvariantOps
import cats.InvariantSemigroupal.ops.toAllInvariantSemigroupalOps
import sandbox.C3_FunctorAndApplicativeFunctor.ContravariantFunctor.Box

trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

trait Invariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

object ContravariantFunctor {
  trait Printable[A] { self =>
    def format(value: A): String
    def contramap[B](func: B => A): Printable[B] = new Printable[B] {
      def format(value: B): String = self.format(func(value))
    }
  }
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)


  implicit val stringPrintable: Printable[String] = (value: String) => s"'${value}'"
  implicit val booleanPrintable: Printable[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  format("hello")
  format(true)

  final case class Box[A](value: A)

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap(_.value)

  import cats.Contravariant
  import cats.Show
  import cats.instances.string._
  import cats.syntax.contravariant._ // for contramap

  val showString = Show[String]
  val showSymbol = showString.contramap((sym: Symbol) => s"'${sym.name}") // Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")
}

object InvariantFunctor {
  trait Codec[A] { self =>
    def encode(value: A): String
    def decode(value: String): A
    def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
      def encode(value: B): String = self.encode(enc(value))
      def decode(value: String): B = dec(self.decode(value))
    }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String = c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A = c.decode(value)

  implicit val stringCodec: Codec[String] = new Codec[String] {
    def encode(value: String): String = value
    def decode(value: String): String = value
  }

  implicit val intCodec: Codec[Int] = stringCodec.imap(_.toInt, _.toString)

  implicit val booleanCodec: Codec[Boolean] = stringCodec.imap(_.toBoolean, _.toString)


  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)


  implicit def boxCodec[A]: Codec[Box[A]] = new Codec[Box[A]] {
    def encode(value: Box[A]): String = value.value.toString
    def decode(value: String): Box[A] = Box(value)
  }

  implicit def boxCodec2[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap(Box(_), _.value)

  import cats.Monoid
  import cats.instances.string._
  import cats.syntax.invariant._
  import cats.syntax.semigroup._

  implicit val symbolMoniod: Monoid[Symbol] = Monoid[String].imap(Symbol.apply)(_.name)
  Monoid[Symbol].empty
  Symbol("a") |+| Symbol("few") |+| Symbol("words")
}