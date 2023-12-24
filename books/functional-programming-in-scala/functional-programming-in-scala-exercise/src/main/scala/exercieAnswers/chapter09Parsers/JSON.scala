package exercieAnswers.chapter09Parsers

import scala.language.implicitConversions

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(s)

    def array: Parser[JSON] =   value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)).surround("[", "]").scope("array")

    def obj: Parser[JSON] = keyval.sep(token(",")).map(kvs => JObject(kvs.toMap)).surround("{", "}").scope("object")

    def lit: Parser[JSON] = (
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
      ).scope("literal")

    def value: Parser[JSON] = lit | obj | array

    def keyval: Parser[(String, JSON)] = escapedQuoted ** (token(":") *> value)

    def document: Parser[JSON] = whitespace  *> (array | obj)

    document
  }
}

object JsonParserExample {

}
