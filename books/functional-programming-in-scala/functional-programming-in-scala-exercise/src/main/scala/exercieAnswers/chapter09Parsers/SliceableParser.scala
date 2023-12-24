package exercieAnswers.chapter09Parsers

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object SliceableParser {
  /** parser is a kind of state action that can fail */
  type Parser[+A] = ParseState => Result[A]

  /**
   * `ParserState` wraps a `Location` and provides some extra convenience function.
   * */
  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState = copy(loc = loc.advanceBy(numChars))
    def input: String = loc.remaining

    def slice(n: Int): String = loc.slice(n)
  }

  sealed trait Result[+A] {
    def mapError(f: ParserError => ParserError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, isCommitted = false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }

    /** increments the number of consumed characters of a successful result */
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }

    def extract: Either[ParserError, A] = this match {
      case Failure(e, _) => Left(e)
      case Success(s, _) => Right(s)
    }
  }
  case class Success[+A](get: A, length: Int) extends Result[A]
  case class Failure(get: ParserError, isCommitted: Boolean) extends Result[Nothing]


  def firstNonMatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i + offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length - offset >= s2.length)
      -1
    else
      s1.length - offset
  }

  object MyParsers extends Parsers[Parser] {
    def run[A](p: Parser[A])(input: String): Either[ParserError, A] = p(ParseState(Location(input))).extract

    def string(s: String): Parser[String] = l => {
      val i = firstNonMatchingIndex(l.input, s, l.loc.offset)
      if (i == -1)
        Success(s, s.length)
      else
        Failure(l.loc.advanceBy(i).toError(s"'$s"), i != 0)
    }

    def regex(r: Regex): Parser[String] = l => r.findPrefixOf(l.loc.remaining) match {
      case None => Failure(l.loc.toError(s"regex: $r"), isCommitted = false)
      case Some(m) => Success(m, m.length)
    }

    def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    def slice[A](p: Parser[A]): Parser[String] = l => p(l) match {
      case Success(_, length) => Success(l.slice(length), length)
      case f@Failure(_, _) => f
    }

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.push(l.loc, msg))
    def label[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.label(msg))

    def attempt[A](p: Parser[A]): Parser[A] = l => p(l).uncommit

    /**
     * If p1 fail in an uncommitted state, try p2
     * else if p1 fail in a committed state, fail early
     * */
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = l => p1(l) match {
      case Failure(_, false) => p2(l)
      case r => r
    }

    def flatMap[A, B](p: Parser[A])(g: A => Parser[B]): Parser[B] = l => p(l) match {
      case Success(a, n) => g(a)(l.advanceBy(n)) // advance source location and call second parser
                            .addCommit(n != 0) // commit if parser has already consumed some input
                            .advanceSuccess(n) // if successful, increase the number of characters consumed by n to account for characters already consumed by f
      case f@Failure(_, _) => f
    }


    def fail(msg: String): Parser[Nothing] = l => Failure(l.loc.toError(msg), isCommitted = true)

    /** Improvement on original many to avoid stack overflow on large inputs */
    override def many[A](p: Parser[A]): Parser[List[A]] = l => {
      val buf = new ListBuffer[A]
      @tailrec
      def go(p: Parser[A], offset: Int): Result[List[A]] = p(l.advanceBy(offset)) match {
        case Success(a, n) => buf += a; go(p, offset + n)
        case f@Failure(_, true) => f
        case Failure(_, _) => Success(buf.toList, offset)
      }
      go(p, 0)
    }
  }
}
