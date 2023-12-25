package exercieAnswers.chapter09Parsers

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex


object SliceableTypes {
  /** parser is a kind of state action that can fail */
  type Parser[+A] = ParseState => Result[A]

  /**
   * `ParserState` wraps a `Location` and provides some extra convenience function.
   * `isSliced` indicates if the current parser is surround by a `slice` combinator.
   * this lets us avoid building up values that will be thrown away
   * */
  case class ParseState(loc: Location, isSliced: Boolean) {
    def advanceBy(numChars: Int): ParseState = copy(loc = loc.advanceBy(numChars))
    def input: String = loc.remaining

    def unslice: ParseState = copy(isSliced = false)
    def reslice(s: ParseState): ParseState = copy(isSliced = s.isSliced)
    def slice(n: Int): String = loc.slice(n)

  }

  /**
   * parse result
   * - Success(a, n): successful parse, a is value, n is # of consumed char
   * - Slice(n): successful slice, n is # of consumed char
   * - Failure(n, isCommitted): fail parse
   * */
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
    def advanceSuccess(n: Int): Result[A]

    def extract(input: String): Either[ParserError, A]
    def slice: Result[String]
  }
  case class Slice(n: Int) extends Result[String] {
    override def extract(input: String): Either[ParserError, String] = Right(input.substring(0, n))
    override def slice: Result[String] = this

    override def advanceSuccess(l: Int): Result[String] = Slice(l + n)
  }
  case class Success[+A](get: A, length: Int) extends Result[A] {
    override def extract(input: String): Either[ParserError, A] = Right(get)

    override def slice: Result[String] = Slice(length)

    override def advanceSuccess(n: Int): Result[A] = Success(get, length + n)
  }
  case class Failure(get: ParserError, isCommitted: Boolean) extends Result[Nothing] {
    override def extract(input: String): Either[ParserError, Nothing] = Left(get)

    override def slice: Result[String] = this

    override def advanceSuccess(n: Int): Result[Nothing] = this
  }


  /**
   * return -1 if s1 startWith s2
   * otherwise return first index where two string differed.
   * If s2 is longer than s1, return s1.length
   * */
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

  object SliceableParser extends Parsers[Parser] {
    def run[A](p: Parser[A])(input: String): Either[ParserError, A] = p(ParseState(Location(input), isSliced = false)).extract(input)

    def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    def fail(msg: String): Parser[Nothing] = l => Failure(l.loc.toError(msg), isCommitted = true)

    /**
     * If p1 fail in an uncommitted state, try p2
     * else if p1 fail in a committed state, fail early
     * */
    def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = l => p1(l) match {
      case Failure(_, false) => p2(l)
      case r => r
    }

    /**
     * `Result` is a Generalized Algebraic Data Type
     * which mean not all constructors of result have the same type
     * In particular, `Slice` _refines_ the `A` type parameters to be `String`.
     * When pattern matching on Result, and obtaining a Slice, we expect compiler to
     * understand that A is in fact a string. unfortunately, scala 2 doesn't support this
     * */
    override def map[A, B](p: Parser[A])(f: A => B): Parser[B] = s => p(s) match {
      case Success(a, n) => Success(f(a), n)
      case Slice(n) => Success(f(s.slice(n).to[A]),n) // asInstanceOf should work but scala was aware of A is string? however, f still require A ???
      case f@Failure(_, _) => f
    }

    /**
     * since we need an `A` to generate a second parser
     * we run the first parser unsliced. once we have A then reslice on the second parser
     * this is done even if flatMap is wrapped in slice call.
     *
     * This could be less efficient if the second parser does not depend on the first (e.g map2)
     * */
    def flatMap[A, B](p: Parser[A])(g: A => Parser[B]): Parser[B] = s => p(s.unslice) match {
      case Success(a, n) =>
        g(a)(s.advanceBy(n).reslice(s)) // advance source location and call second parser
          .addCommit(n != 0) // commit if parser has already consumed some input
          .advanceSuccess(n) // if successful, increase the number of characters consumed by n to account for characters already consumed by f
      case f@Failure(_, _) => f
      case Slice(n) => g(s.slice(n).to[A])(s.advanceBy(n).reslice(s)).advanceSuccess(n)
    }
    def string(input: String): Parser[String] = s => {
      val i = firstNonMatchingIndex(s.input, input, s.loc.offset)
      if (i == -1) { // match
        if (s.isSliced)
          Slice(input.length)
        else
          Success(input, input.length)
      } else
        Failure(s.loc.advanceBy(i).toError(s"\'$input\'"), i != 0)
    }

    def regex(r: Regex): Parser[String] = s => r.findPrefixOf(s.loc.remaining) match {
      case None => Failure(s.loc.toError(s"regex: $r"), isCommitted = false)
      case Some(m) => if (s.isSliced) Slice(m.length)else Success(m, m.length)
    }

    def slice[A](p: Parser[A]): Parser[String] = s => p(s.copy(isSliced = true)).slice

    def scope[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.push(l.loc, msg))
    def label[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.label(msg))

    def attempt[A](p: Parser[A]): Parser[A] = l => p(l).uncommit

    override def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = s => p(s) match {
      case Success(a,n) => val s2 = s.advanceBy(n); p2(s2) match {
        case Success(b, m) => Success(f(a, b), n+m)
        case Slice(m) => Success(f(a, s2.slice(m).to[B]), n+m)
        case f@Failure(_, _) => f
      }
      case Slice(n) => val s2 = s.advanceBy(n); p2(s2) match {
        case Success(b, m) => Success(f(s2.slice(m).to[A],b), n+m)
        case Slice(m) => if (s.isSliced) Slice(n+m).asInstanceOf[Result[C]] else Success(f(s.slice(n).to[A], s2.slice(n).to[B]), n+m)
        case f@Failure(_, _) => f
      }
      case f@Failure(_, _) => f
    }

    override def product[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = map2(p1, p2)(_ -> _)


    /** Improvement on original many to avoid stack overflow on large inputs */
    override def many[A](p: Parser[A]): Parser[List[A]] = s => {
      if (s.isSliced) { // avoid building up List of char if it's sliced
        @tailrec
        def go(p: Parser[String], offset: Int): Result[String] = p(s.advanceBy(offset)) match {
          case f@Failure(_, true) => f
          case Failure(_, _) => Slice(offset)
          case Slice(n) => go(p, offset + n)
          case Success(_, _) => sys.error("sliced parser should not return success, only slice")
        }
        go(p.slice, 0).asInstanceOf[Result[List[A]]]
      } else {
        val buf = new ListBuffer[A]
        @tailrec
        def go(p: Parser[A], offset: Int): Result[List[A]] = p(s.advanceBy(offset)) match {
          case Success(a, n) => buf += a; go(p, offset + n)
          case f@Failure(_, true) => f
          case Failure(_, _) => Success(buf.toList, offset)
          case Slice(n) => buf += s.slice(n); go(p, offset + n)
        }
        go(p, 0)
      }
    }
  }
}

