package exercieAnswers.chapter14LocalEffect

import exercieAnswers.chapter14LocalEffect.Mutable.STArray.fromList

import scala.collection.mutable

/**
 * * with [[ST]] monad, the plan is to build up a computation that when run allocate some local mutable state,
 * * accomplish some task then discard the mutable state. since all mutable state is locally scoped, the task
 * * is considered referentially transparent. for that we need to guarantee that we can never extract
 * [[STRef]] or [[STArray]] or [[STMap]] out of an ST
 *
 * We want to disallow running an action of type `ST[S, STRef[S,A]]` because that
 * would expose the STRef. And in general we want to disallow running any ST[S,T]
 * where T involves the type S. Since we don't care what S actually is, the action might as well by polymorphic
 * in S. we represent it by introducing [[RunnableST]]
 *
 * the only way to create a mutable references either of type [[STRef]] or [[STArray]] or [[STMap]]
 * is through using the provided apply method: [[STRef.apply]], [[STArray.apply]], [[STMap.apply]]
 * which does not directly return a mutable reference but once wrapped in [[ST]]
 * */
object Mutable {
  def quickSort(xs: Seq[Int]): Seq[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(a: Int, b: Int): Unit = {
      val tmp = arr(a)
      arr(a) = arr(b)
      arr(b) = tmp
    }

    def partition(l: Int, h: Int) = {
      // use right most element as pivot
      val pivotVal = arr(h)

      // pointer for greater element. will move 1 position when we find it
      var i = l - 1

      // for each element from l to h
      for (j <- l until h) {
        // if an element is lower than pivot then swap it with
        // the greater element at index i
        if (arr(j) < pivotVal) {
          i += 1
          swap(j, i)
        }
      }
      // swap pivot with greater element
      i += 1
      swap(i, h)
      i
    }

    def qs(l: Int, h: Int): Unit = {
      if (l < h) {
        val pi = partition(l, h)
        qs(l, pi - 1)
        qs(pi + 1, h)
      }
    }

    qs(0, arr.length - 1)
    arr.toList
  }


  sealed trait ST[S, A] { self =>
    protected def run(s: S): (A, S)
    def map[B](f: A => B): ST[S, B] = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        (f(a), s1)
      }
    }

    def flatMap[B](f: A => ST[S, B]): ST[S, B]  = new ST[S, B] {
      def run(s: S): (B, S) = {
        val (a, s1) = self.run(s)
        f(a).run(s1)
      }
    }
  }

  object ST {
    def apply[S, A](a: => A):ST[S, A] = {
      lazy val memo = a
      new ST[S, A] {
        def run(s: S): (A, S) = (memo, s)
      }
    }

    /**

     * * Note: this must be on the [[ST]] companion object so it can access protected [[ST.run]]
     * */
    def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
  }

  sealed trait STRef[S, A] {
    protected var cell: A
    def read: ST[S, A] = ST(cell)
    def write(a: => A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        cell = a
        ((), s)
      }
    }
  }

  object STRef {
    def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(
      new STRef[S, A] {var cell: A = a}
    )
  }

  /**
   * Because Type S is bound on the apply method, it isn't possible to create a `RunnableST[STRef[Nothing, Int]]`
   * any mutable STRef type that rely on S of the ST action it live in will not be able to
   * escape - guaranteed by scala type system.
   * and since STRef are sure to be locally scoped within a ST action, mutating the reference is now safe
   * */
  trait RunnableST[A] {
    def apply[S]: ST[S, A]
  }

  val p = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x <- r1.read
      y <- r2.read
      _ <- r1.write(y)
      _ <- r2.write(x)
      a <- r1.read
      b <- r2.read
    } yield (a, b)
  }
  val r = ST.runST(p)

  /**
   * Mutable References on their own aren't at all usefull.
   * Mutable Array are much more compelling usage of [[ST]] monad
   * */
  sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) { // Manifest is required for Array creation
    protected def value: Array[A]
    def size: ST[S, Int] = ST(value.length)
    def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        value(i) = a
        ((), s)
      }
    }

    def read(i: Int): ST[S, A] = ST(value(i))

    def freeze: ST[S, List[A]] = ST(value.toList)

    def fill(xs: Map[Int, A]): ST[S, Unit] = new ST[S, Unit] {
      def run(s: S): (Unit, S) = {
        xs.foreach { case (idx, el) => value(idx) = el }
        ((), s)
      }
    }

    /**
     * with a bit of mental gymnastic we could use foldLeft and above [[write]]
     */
    def fill_(xs: Map[Int,A]): ST[S,Unit] =
      xs.foldLeft(ST[S,Unit](())) { case (st, (k, v)) => st flatMap (_ => write(k, v)) }


    def swap(i: Int, j: Int): ST[S, Unit] = for {
      x <- read(i)
      y <- read(j)
      _ <- write(i, y)
      _ <- write(j, x)
    } yield ()
  }

  object STArray {
    def apply[S, A: Manifest](sz: Int, v: A): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] { lazy val value: Array[A] = Array.fill(sz)(v) }
    )

    def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(
      new STArray[S, A] { lazy val value: Array[A] = xs.toArray }
    )
  }

  def partition[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Int] = for {
    pivot <- a.read(r)
    i <- STRef(l - 1)
    _ <- (l until r).foldLeft(ST[S, Unit](()))((s, j) => for {
      vj <- a.read(j)
      _ <- if  (vj < pivot) {
        for {
          vi <- i.read
          _ <- a.swap(vi + 1, vj)
          _ <- i.write(vi + 1)
        }  yield()
      } else ST[S, Unit](())
    } yield ())
    vi <- i.read
    _ <- a.swap(vi + 1, r)
    _ <- i.write(vi + 1)
    fi <- i.read
  } yield fi


  def qs[S](a: STArray[S, Int], l: Int, r: Int): ST[S, Unit] =
    if (l < r)
      for {
        pi <- partition(a, l, r)
        _ <- qs(a, l, pi - 1)
        _ <- qs(a, pi + 1, r)
      } yield ()
    else
      ST[S, Unit](())

  def quickSort(xs: List[Int]): List[Int] = {
    if (xs.isEmpty)
      xs
    else
      ST.runST(new RunnableST[List[Int]] {
        def apply[S]: ST[S, List[Int]] = for {
          arr <- fromList(xs)
          s <- arr.size
          _ <-  qs(arr, 0, s)
          sorted <- arr.freeze
        } yield sorted
      })
  }

  sealed trait STMap[S, K, V] {
    protected def table: mutable.HashMap[K,V]
    def size: ST[S, Int] = ST(table.size)
    def apply(k: K): ST[S, V] = ST(table(k))
    def get(k: K): ST[S, Option[V]] = ST(table.get(k))
    def += (kv: (K, V)): ST[S, Unit] = ST(table += kv)
    def -= (kv: (K, V)): ST[S, Unit] = ST(table -= kv)
  }

  object STMap {
    def apply[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = fromMap(m)
    def empty[S, K, V]: ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
      val table = mutable.HashMap.empty[K, V]
    })

    def fromMap[S, K, V](m: Map[K, V]): ST[S, STMap[S, K, V]] = ST(new STMap[S, K, V] {
      val table = (mutable.HashMap.newBuilder[K, V] ++= m).result()
    })
  }
}
