package sandbox

// type class represents a set of functionality that we want to implement

// for example we want to write serialize to json functionality
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
case object JsNull extends Json

// This is our type class
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// This is our type class instances
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    (value: String) => JsString(value)

  implicit val personWriter: JsonWriter[Person] =
    (value: Person) => JsObject(
      Map(
        "name" -> JsString(value.name),
        "email" -> JsString(value.email)
      )
    )

  // This allow implicit recursive resolution so we can just combine different type class instances
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    {
      case Some(aValue) => writer.write(aValue)
      case None => JsNull
    }
}

// interface object - an interface that use a type class
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = w.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json = w.write(value)
  }
}


object Example {
  // in both cases compiler can search for correct parameters
  import JsonWriterInstances._
  val testPerson =Person("Steve", "Steve@gmail.com")
  Json.toJson(testPerson)

  import JsonSyntax._
  testPerson.toJson

  // demonstrate recursive implicit resolution
  Json.toJson(Option(testPerson))

  // implicitly can summon any value from implicit scope. good for debugging
  implicitly[JsonWriter[String]]
}