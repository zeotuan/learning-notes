package exercieAnswers.chapter07Parallelism

import java.util.concurrent.{Callable, ExecutorService}
import scala.util.{Failure, Try}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.annotation.tailrec

final class Actor[A](strategy: Strategy)(handler: A => Unit, onError: Throwable => Unit = throw(_)) {
  self =>

  private val tail = new AtomicReference(new Node[A]()) // tail of message queue
  private val suspended = new AtomicInteger(1)  // indicate whether actor is suspended or not
  private val head = new AtomicReference(tail.get) // head of message queue

  /**
   * Alias for `apply`
   * pass the message `a` to the mail box of this actor
   */
  def !(a: A): Unit = {
    val n = new Node(a) // create new node with message
    head.getAndSet(n).lazySet(n)
  }

  def apply(a: A): Unit = {
    this ! a
  }

  // actor is suspended, unsuspended it and schedule
  private def trySchedule(): Unit = if (suspended.compareAndSet(1, 0)) schedule()

  // use actor scheduling strategy to act
  private def schedule(): Unit = strategy(act())

  // contravariant mapping which create new actor of type b
  // by applying function f to a
  // which send message back to this actor
  def contramap[B](f: B => A): Actor[B] = new Actor[B](strategy)((b: B) => this ! f(b), onError)

 /** Actor Message Processing */
  private def act(): Unit = {
    val t = tail.get // get tail
    val n = batchHandle(t, 1024) // process message in batch of 1024
    if (n ne t) { // if queue is not empty then update tail and schedule further processing
      n.a = null.asInstanceOf[A]
      tail.lazySet(n)
      schedule()
    } else {
      suspended.set(1) // else suspend, try schedule if more message are in the queue
      if (n.get ne null) trySchedule()
    }
  }
  @tailrec
  private def batchHandle(t: Node[A], i: Int):Node[A] = {
    val n = t.get
    if (n ne null) {

      Try(handler(n.a)) match {
        case Failure(e: Throwable) => onError(e)
        case _ =>
      }

      if (i > 0) batchHandle(n, i - 1) else n
    } else t
  }
}

object Actor {
  def apply[A](es: ExecutorService)(handler: A => Unit, onError: Throwable => Unit = throw(_)): Actor[A] = new Actor[A](Strategy.fromExecutorService(es))(handler, onError)
}

trait Strategy {
  def apply[A](a: => A): () => A
}

object Strategy {
  def fromExecutorService(es: ExecutorService): Strategy = new Strategy {
    override def apply[A](a: => A): () => A = {
      val f = es.submit { new Callable[A] { def call = a }}
      () => f.get()
    }
  }

  def sequential: Strategy = new Strategy {
    override def apply[A](a: => A): () => A = {
      val r = a
      () => r
    }
  }
}

private class Node[A](var a: A = null.asInstanceOf[A]) extends AtomicReference[Node[A]] // Representing a node in the non-intrusive MPSC queue