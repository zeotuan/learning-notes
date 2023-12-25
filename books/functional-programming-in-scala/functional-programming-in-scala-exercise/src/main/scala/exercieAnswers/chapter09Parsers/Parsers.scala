package exercieAnswers.chapter09Parsers

import exercieAnswers.chapter08PropertyTesting.{Gen, Prop, SGen}

import java.util.regex.Pattern
import scala.language.implicitConversions
import scala.util.matching.Regex


trait Parsers[Parser[+_]] { self =>

  def run[A](p: Parser[A])(input: String): Either[ParserError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))

  /** chooses between two parsers, first attempting p1 then p2 if p1 fail in an uncommitted state */
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  /** Recognize and return single string*/
  implicit def string(s: String): Parser[String]

  /** recognizes a regular expression s */
  implicit def regex(r: Regex): Parser[String]
  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f:A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  /** Apply f to result of parser a if successful */
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(f andThen succeed)

  /**
   * This Parsers always succeed with the value a, regardless of input string
   * */
  def defaultSucceed[A](a: A): Parser[A] = string("").map(_ => a)

  /**
   * This Parsers always succeed with the value a, regardless of input string
   * */
  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  /**
   * delay commit to p until after it succeed
   * should satisfy attempt (p.flatMap(_ => fail)) or p2 == p2
   * */
  def attempt[A](p: Parser[A]): Parser[A]

  /**
   * return portion of input string examined by parser if successful
   * (char('a')| char('b')).many.slice.run("aaba") == Right("aaba")
   * */
  def slice[A](p: Parser[A]): Parser[String]

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n <= 0) succeed(List()) else map2(p, listOfN(n - 1, p))(_ :: _)

  /** Sequences two parsers, running p1 then p2 and return pair of result if both succeed */
  def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = p1.flatMap(a => p2.map(b => (a, b)))
  def productViaMap2[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = p1.map2(p2)((a, b) => (a, b))


  def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = p.flatMap(a => p2.map(b => f(a,b)))
  // Exercise 9.1: Implement map2 then many and many1 in term of map2
  def map2ViaProduct[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2).map { case (a, b) => f(a,b) }


  /**
   * recognizes ZERO or more repetitions of char 'a' and return the number of char it has seen
   * since many implementation call itself recursively, map2 and product must be non strict in it argument
   * this impl might suffer from stack overflow
   * */
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) | succeed(List())

  /**
   * recognizes ONE or more repetitions of char 'a' and return the number of char it has seen
   * */
  def many1[A](p: Parser[A]): Parser[List[A]] =  map2(p, many(p))(_ :: _)

  // def defer[A](p: => Parser[A]): Parser[A]

  /** Run a parsers the use  result to select a second parser to run in sequence */
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]


  /** run 2 parsers but drop the left result*/
  def skipL[A, B](p1: Parser[A], p2: Parser[B]): Parser[B] = p1.map2(p2)((_, b) => b)

  /** run 2 parsers but drop the right result*/
  def skipR[A, B](p1: Parser[A], p2: Parser[B]): Parser[A] = p1.map2(p2)((a, _) => a)

  /** combine 'p' with the result of many 's *> p' */
  def sep1[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] = p.map2((s *> p).many)(_ :: _)
  def sep[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] = p.sep1(s) | succeed(Nil)
  def token(s: String): Parser[String] = string(s).token

  /** in the event of failure add msg to the error stack return by p */
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  /** In the event of failure, replaces the assigned message with msg */
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def whitespace: Parser[String] = "\\s*".r

  /** end of input parser */
  def eof: Parser[String] = "\\z".r

  /** 1 or more digits parser */
  def digits: Parser[String] = regex("\\d+".r)

  /** Parser which consumes reluctantly until it encounter given string */
  def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] = quoted.label("string literal").token //TODO: unable to handle escaped string literals. use unescaped quite for now

  /**
   * C style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc
   * use string to keep result full precision
   * */
  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  /** convert floating point literals to `Double` */
  def double: Parser[Double] = doubleString.map(_.toDouble).label("double literal")

  def surround[A](start: Parser[Any], end: Parser[Any])(p: Parser[A]): Parser[A] = start *> p <* end

  case class ParserOps[A](p: Parser[A]) {
    def listOfN(n: Int): Parser[List[A]] = self.listOfN(n, p)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def |[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def many: Parser[List[A]] = self.many(p)

    def many1: Parser[List[A]] = self.many1(p)

    def slice: Parser[String] = self.slice(p)

    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = product(p2)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] = self.map2(p, p2)(f)

    /**
     * runs a parsers and then use it's result to select a second parser to run.
     * with flatMap we can implement context-sensitive grammar
     * for example: Parsing a single digit '4' followed by many 'a' character
     * input for this parser could be "0", "1a", "4aaaaa"
     * */
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def *>[B](p2: => Parser[B]): Parser[B] = self.skipL(p, p2)

    def <*[B](p2: => Parser[B]): Parser[A] = self.skipR(p, p2)

    def sep1(sep: Parser[Any]): Parser[List[A]] = self.sep1(p, sep)

    def sep(sep: Parser[Any]): Parser[List[A]] = self.sep(p, sep)

    def attempt: Parser[A] = self.attempt(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)

    def token: Parser[A] = p.attempt <* whitespace

    def surround(start: Parser[Any], end: Parser[Any]): Parser[A] = self.surround(start, end)(p)

  }


  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    /** map(p)(a => a) == p */
    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop = equal(p, p.map(a => a))(in)

    /**
     * (a ** b) ** c
     * a ** (b ** c)
     * ~= is used when there is an obvious bijection between two sides
     * (a ** b) ** c ~= a ** (b ** c)
     * */
    def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
    def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

    def associative[A, B, C](a: Parser[A], b: Parser[B], c: Parser[C]): Boolean = ((a ** b) ** c).map(unbiasL) == (a ** (b ** c)).map(unbiasR)

    def associative[A, B, C, D](a: Parser[A], b: Parser[B])(f: A => C, g: B => D): Boolean = {
      val ass1: Parser[(C, D)] = a.map(f) ** b.map(g)
      val ass2: Parser[(C, D)] = (a ** b).map { case (a,b) => (f(a), g(b))}
      ass1 == ass2
    }

    // map and product also have an interesting relationship; we can map either before or after taking the product of two parsers without changing the behaviours
    def errorLocation(e: ParserError): Location = ???
    def errorMessage(e: ParserError): String = ???

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop = Prop.forAll(inputs ** Gen.string) {
      case (i, msg) => run(label(msg)(p))(i) match {
        case Left(e) => errorMessage(e) == msg
        case _ => true
      }
    }
  }


  object Example {
    val numA: Parser[Int] = char('a').many.map(_.size)
    assert(run(numA)("aaa") == Right(3))

    // numA2 has O(1) when accessing size as String.size is constant
    // numA has O(n) when accessing size due to List.size
    val numA2: Parser[Int] = char('a').many.slice.map(_.size)
    assert(run(numA2)("aaa") == Right(3))

    val zeroOrMoreA_And_OneOrMoreB = char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)

    // Exercise 9.6:
    // Parse one or more digit and succeed if it's can be successfully convert to integer
    val nonNegativeInt: Parser[Int] = regex("[0-9]+".r).flatMap(_.toIntOption.map(succeed).getOrElse(fail("Expected an integer")))

    //
    val nConsecutiveAs: Parser[Int] = nonNegativeInt.flatMap(n => char('a').listOfN(n).map(_ => n)) // return the number of parsed 'a' instead of the parsed string

    // Here after parsing the first "abra", we don't know whether to expect another "abra" or "cadabra!". attempt allow the second branch to be considered up until
    // we have finished parsing the second "abra".
    run((("abra" ** whitespace ** "abra").attempt ** "cadabra") or ("abra" ** whitespace ** "cadabra"))("abra cadabra!")
  }
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParserError = ParserError(List((this, msg)))

  /** increment offset by n */
  def advanceBy(n: Int): Location = copy(offset = offset + n)

  /** get the remaining available input */
  def remaining: String = input.substring(offset)

  def slice(n: Int): String = input.substring(offset, offset + n)

  def currentLine: String = if (input.length > 1)
    input.linesIterator.drop(line - 1).next()
  else
    ""
  def columnCaret = (" " * (col - 1)) + "^"
}

case class ParserError(stack: List[(Location, String)] = List(), otherFailures: List[ParserError] = Nil) {
  def label[A](s: String): ParserError = ParserError(latestLoc.map(_ -> s).toList)

  def latest: Option[(Location, String)] = stack.lastOption
  def latestLoc: Option[Location] = latest.map(_._1)

  def push(loc: Location, msg: String): ParserError = copy(stack = (loc, msg) :: stack)

  /**
   * use the groupBy function to create a `Map[Location, List[(Location, String)]]`
   * concatenate each inner list error message into single `;` separated string
   * then sort by where error happen in input string
   * */
  def collapseStack: List[(Location, String)] = stack
    .groupBy(_._1)
    .view
    .mapValues(_.map(_._2).mkString("; "))
    .toList.sortBy(_._1.offset)


  override def toString: String = if (stack.isEmpty)
    "no error message"
  else {
    lazy val collapsed = collapseStack
    // context: the line of input that failed to parse along with a caret pointing to the character in that line where the error occurred
    val context = collapsed.lastOption.map(e => "\n\n" + e._1.currentLine).getOrElse("") + collapsed.lastOption.map(e => "\n" + e._1.columnCaret).getOrElse("")
    // print each error in the collapse along with the line and col the error occurred
    collapsed.map { case (loc, msg) => loc.line.toString + "." + loc.col + " " + msg}.mkString("\n") + context
  }

  /** store other error occur when parsing */
  def addFailure(e: ParserError): ParserError = copy(otherFailures = e::otherFailures)

}
