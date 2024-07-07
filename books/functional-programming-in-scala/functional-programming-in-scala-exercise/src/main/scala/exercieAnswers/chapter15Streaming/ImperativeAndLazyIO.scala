package exercieAnswers.chapter15Streaming

import exercieAnswers.chapter13IO.FreeTest.Console.IO

import java.io._

object ImperativeIO {

  /** Imperative implementation which is low level and not really compositional */
  def linesGt40k(filename: String): IO[Boolean] = IO {
    val src = io.Source.fromFile(filename)
    try {
      var count = 0
      val lines: Iterator[String] = src.getLines()
      while (count <= 40000 && lines.hasNext) {
        lines.next()
        count += 1
      }
      count > 40000
    } finally  {
      src.close()
    }
  }


  /** Lazy IO alternative */
  def linesGt40kViaLazyIO(filename: String): IO[Int] = {
    def lines(filename: String): IO[LazyList[String]] = IO {
      val src = io.Source.fromFile(filename)
      src.getLines().to(LazyList).lazyAppendedAll {
        src.close()
        LazyList.empty
      }
    }


    /** We can now perform different function on the lazyList string without having to worry about IO stuff */
    lines(filename).flatMap(
      l => IO {
        l.filter(_.trim.nonEmpty).take(40000).map(_.head).indexOfSlice("abracadabra".toList)
      }
    )

    /**
     * This is called Lazy IO and it can be problematic:
     * - not resource safe: resource will only be closed if we examine and reach the end of the stream
     * - Nothing stop us from traversing the same lazyList multiple time which will cause either:
     *  + excessive memory usage as all element is cached in memory
     *  + if result is not cached then we get error from reading closed file
     * - forcing elements of stream has I/O side effect which will cause unpredictable result when two
     *  threads traversing the LazyList at the same time
     * - in more realistic scenarios, the LazyList[String] usage is unpredictable. It might be used in a data structure
     *  which will store it for long period of time before it is examined. using this piece of data is not as easy any
     *  more as it require inside knowledge. This make it bad for composition where we shouldn't need knowledge about
     *  anything other than the type
     * */
  }
}