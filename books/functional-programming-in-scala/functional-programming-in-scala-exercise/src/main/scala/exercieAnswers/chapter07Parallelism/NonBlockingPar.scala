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
  }
}
