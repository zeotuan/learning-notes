package sandbox.C1_TypeClass

object Variance {
  sealed trait Shape
  case class Circle(radius: Double) extends Shape

  // F[B] is subtype of F[A] if B is subtype of A
  object CoVariant {
    trait List[+A] { }
    trait Option[+A] { }

    val circles: List[Circle] = new List[Circle] {}
    val shapes: List[Shape] = circles
  }

  object ContraVariant {
    // F[A] is a subtype of F[B] of B is a subtype of A
    trait ConTraJsonWriter[-A] {
      def write(value: A): String
    }

    val shape: Shape = ???
    val circle: Circle = ???
    val shapeWriter: ConTraJsonWriter[Shape] = ???
    val circleWriter: ConTraJsonWriter[Circle] = ???

    def format[A](value: A, writer: ConTraJsonWriter[A]): String = writer.write(value)

    format(circle, shapeWriter)
    format(circle, circleWriter)
    format(shape, shapeWriter)
    // format(shape, circleWriter) not allow since shape is not subtype of circle
    // This is the relationship that contravariant represents
    // ConTraJsonWriter[Shape] is a subtype of ConTraJsonWriter[Circle]
    // because Circle is a subtype of Shape
    // ConTraJsonWriter[Shape] can be used anywhere a ConTraJsonWriter[Circle] is expected
  }

  /**
   * Variance Type:              Invariant    Covariant    Contravariant
   * SuperType instance used:    No           No           Yes
   * Prefer specific instance:   No           Yes          No
   *
   * Invariant is preferred with cat allowing specify more specific  instances for subtypes if we want
   * */
}

