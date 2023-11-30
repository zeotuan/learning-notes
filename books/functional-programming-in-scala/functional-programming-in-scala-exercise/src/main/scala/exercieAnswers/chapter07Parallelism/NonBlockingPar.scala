package exercieAnswers.chapter07Parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object NonBlockingPar {
  type Future[+A] = (A => Unit) => Unit // sometimes call callback or continuation
  type Par[A] = ExecutorService => Future[A]

  object Par {
    def run[A](es: ExecutorService)(pa: Par[A]): A = {
      val ref = new AtomicReference[A]() // mutable thread safe reference to store the result
      val latch = new CountDownLatch(1) // allow thread to wait until countDown is called certain number of time (1 in our case since we want to block only until we get a)
      pa(es)(a => { // set result and countDown latch to release blocking
        ref.set(a)
        latch.countDown()
      })
      latch.await()
      ref.get()
    }

    def unit[A](a: A): Par[A] = es => cb => cb(a) // simply pass a to the callback

    /** lazy version of unit */
    def delay[A](a: => A): Par[A] = es => cb => cb(a)

    def fork[A](a: => Par[A]): Par[A] = es => cb => eval(es)(a(es)(cb))

    def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {def call: Unit = r})

    /**
     * Helper function for constructing `Par` values out of calls to non-blocking continuation-passing-style APIs.
     */
    def async[A](f: (A => Unit) => Unit): Par[A] = es => cb => f(cb)

    def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = es => cb => {
      // mutable states to store Par result
      var ar: Option[A] = None
      var br: Option[B] = None

      //  an actor to await both result, apply f then pass to callback
      //  if A finish first, store in ar. If B result come and we already have A, call f and pass c to callback
      // if B finish first, store in br. If A result come and we already have B, call f and pass c to callback
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => if (br.isDefined) eval(es)(cb(f(a, br.get))) else ar = Some(a)
        case Right(b) => if (ar.isDefined) eval(es)(cb(f(ar.get, b))) else br = Some(b)
      }

      p1(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }

    def map[A, B](p: Par[A])(f: A => B): Par[B] = es => cb => p(es)(a => eval(es)(cb(f(a))))
    def mapViaMap2[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a, _) => f(a))

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case Nil => unit(Nil)
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

    def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (as.isEmpty) unit(Nil)
      else if (as.length == 1) map(as.head)(a => Vector(a))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def sequence[A](as: List[Par[A]]): Par[List[A]] = map(sequenceBalanced(as.toIndexedSeq))(_.toList)

    def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] = sequenceBalanced(as.map(asyncF(f)))
    def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = sequence(as.map(asyncF(f)))

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = es => cb => {
      cond(es)(b => if (b) eval(es)(t(es)(cb)) else eval(es)(f(es)(cb)) )
    }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => cb => n(es)(index => eval(es)(choices(index)(es)(cb)))

    def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = es => cb => key(es)(k => eval(es)(choices(k)(es)(cb)))

    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = es => cb => pa(es)(a => eval(es)(f(a)(es)(cb)))

    def choiceViaFlatMap[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(b => if (b) t else f)
    def choiceNViaFlatMap[A](index: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(index)(choices(_))
    def choiceMapViaFlatMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = flatMap(key)(choices(_))

    def join[A](ppa: Par[Par[A]]): Par[A] = es => cb => ppa(es)(pa => eval(es)(pa(es)(cb)))

    def joinViaFlatMap[A](ppa: Par[Par[A]]): Par[A] = flatMap(ppa)(identity)
    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = join(map(pa)(f))
  }
}


object Example {
}
