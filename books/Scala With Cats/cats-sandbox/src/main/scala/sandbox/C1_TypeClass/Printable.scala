package sandbox.C1_TypeClass

import sandbox.C1_TypeClass.JsonSyntax.PrintableOps


// This is our type class
trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(value: String): String = value
  }

  implicit val intPrintable = new Printable[Int] {
    def format(value: Int): String = value.toString
  }

  implicit val catPrintable = new Printable[Cat] {
    def format(value: Cat): String = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"$name is a $age years old $color cat"
    }
  }
}

final case class Cat(name: String, age: Int, color: String) {
  override def equals(other: Any): Boolean = other match {
    case c: Cat => name == c.name && age == c.age && color == c.color
    case _ => false
}

// interface object - an interface that use a type class
object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
  def print[A](value: A)(implicit p: Printable[A]): Unit = println(format(value))
}

object JsonSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = Printable.format(value)
    def print(implicit p: Printable[A]): Unit = Printable.print(value)
  }
}


object Example {
  // in both cases compiler can search for correct parameters
  import PrintableInstances._
  val testCat =Cat("Steve", 1, "red")
  testCat.print


  // implicitly can summon any value from implicit scope. good for debugging
  implicitly[Printable[Cat]]
}