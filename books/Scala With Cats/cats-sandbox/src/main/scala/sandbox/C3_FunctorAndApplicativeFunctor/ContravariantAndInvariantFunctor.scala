package sandbox.C3_FunctorAndApplicativeFunctor

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
}