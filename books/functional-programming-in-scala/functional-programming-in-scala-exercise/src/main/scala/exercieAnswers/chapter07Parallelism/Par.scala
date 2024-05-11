package exercieAnswers.chapter07Parallelism

import java.util.concurrent._
object Par {
  type Par[A] = ExecutorService => Future[A]
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = _ => UnitFuture(a) // simply wrap value within a UnitFuture without using the ExecutorService

  // Simple Implementation of Future that wrap a constant value. get simply return that constant value
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get // This Implementation of future does not respect timeout
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // map2 does not evaluate call to `f` in a separate logical thread, in accord with our design choice of having fork
  // as the solve function in the API for controlling Parallelism. if we need f to occur in a separate logical thread
  // we can do `fork(map2(a,b)(f))`
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = es => {
    val af = a(es)
    val bf = b(es)
    UnitFuture(f(af.get, bf.get)) // for a implementation that support timeout, we need different Future Impl compare to UnitFuture
  }

  // Simplest implementation of fork
  // some problems
  // - outer Callable block will wait for inner task to complete. since this blocking is happening in our thread pool or whatever
  // resource backend used by executorService, we lose some potential parallelism
  //
  def fork[A](a: => Par[A]): Par[A] = es => es.submit(() => a(es).get)

  // Exercise 7.3: support Timeout in map2
  def map2Timeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = es => {
    new Future[C] {
      private val af = a(es)
      private val bf = b(es)
      @volatile private var cache: Option[C] = None

      override def cancel(mayInterruptIfRunning: Boolean): Boolean = af.cancel(mayInterruptIfRunning) || bf.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

      override def isDone: Boolean = cache.isDefined

      override def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

      override def get(timeout: Long, unit: TimeUnit): C = {
        val timeoutNs = TimeUnit.NANOSECONDS.convert(timeout, unit)
        val started = System.nanoTime
        val a = af.get(timeoutNs, TimeUnit.NANOSECONDS)
        val elapsed = System.nanoTime - started
        val b = bf.get(timeoutNs - elapsed, TimeUnit.NANOSECONDS)
        val c = f(a,b)
        cache = Some(c)
        c
      }
    }
  }

  def lazyUnit[A](a: => A):Par[A] = fork(unit(a))


  /** Exercise 7.4: Write a function to convert any function A => B to one that evaluates it result asynchronously */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // without having to run parList we can use map2 to directly gain access to List within Par
  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map2(parList, unit(()))((a, _) => a.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

  def sortParViaMap(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  // Exercise 7.5: Implement Sequence
  def sequenceViaFoldRight[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(Nil))((h,t) => map2(h,t)(_ :: _))

  // fork the recursive call to new logical step for parallel processing
  // this essentially make sequenceRight recursive
  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
  }

  // using map2 to optimize sequence
  def sequenceBalanced[A](pas: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = if (pas.isEmpty) {
    unit(Nil)
  } else if (pas.size == 1) {
    map(pas.head)(IndexedSeq(_))
  } else {
    val (l, r) = pas.splitAt(pas.size/2)
    map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  // the whole implementation is wrapped in another fork
  // so that the creation of fbs is also done in a logical
  // thread instead of being performed immediately on the
  // calling thread when parMap is called
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork(e => {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)(e)
  })


  // Exercise 7.6: Implement parFilter
  // easy Implementation but all filtering is done on the same thread
  def parSameThreadFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = lazyUnit(as.filter(f))
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork(e => {
    val pars = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(pars))(_.flatten)(e)
  })

  // Law of Mapping
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)
  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = equal(p, p2)(e).get
  assert(map(unit(1))(_ + 1) == unit(2))

  /**
   * unit(1).map(_ + 1) == unit(2)
   * unit(x).map(f) == unit(f(x))
   * unit(x).map(id) == unit(id(x)) // substitute f with identity function
   * unit(x).map(id) == unit(x)     // simplify
   * y.map(id) == y                 // substitute unit(x) with y
   */

  /**  Exercise 7.7: prove that y.map(g).map(f) == y.map(f compose g)
   * y.map(id).map(f compose g) == y.map((f compose g) compose id)  - substitute (f -> id) and (g -> f compose g)
   * y.map(id).map(f compose g)  == y.map(f compose g)
   * y.map(f compose g) == y.map(f compose g)
   */

  /**
   * Exercise 7.8: our implementation doesn't work work with FixedThreadPool when there is only 1 thread
   * it will cause deadlock
   * val a = lazyUnit(43)
   * val es = Executors.newFixedThreadPool(1)
   * println(Par.equal(es)(a, fork(a)))
   *
   * this happen because fork submit a callable
   * and within that callable we submit another callable
   */


  // fix fork to avoid deadlock
  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => if (cond(es).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => choices(n(es).get / choices.size)(es)

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => choices(key(es).get)(es)

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => f(pa(es).get)(es)


  def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(b => if(b) t else f)
  def choiceNViaFlatMap[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(choices(_))
  def choiceMapViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = flatMap(key)(choices(_))

  def join[A](ppa: Par[Par[A]]): Par[A] = es => ppa(es).get()(es)
  def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(identity)
  def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))
}
