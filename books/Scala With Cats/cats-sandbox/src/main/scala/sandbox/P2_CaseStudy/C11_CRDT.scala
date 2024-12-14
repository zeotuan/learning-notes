package sandbox.P2_CaseStudy

import cats.implicits.catsSyntaxSemigroup
import cats.kernel.CommutativeMonoid
import cats.instances.list._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.syntax.foldable._
import sandbox.P2_CaseStudy.KeyValueStore.KvsOps // for combineAll


/**
 * Commutative Replicated Data Type (CRDT) : A type of data structure that can reconcile eventual consistent data
 *
 * consistent system is easy to work with and reason about but have several limitations:
 * - it is hard to scale
 * - it is hard to make highly available
 * - it is hard to make fault-tolerant
 *
 *
 * eventual consistency allow replicas to diverge (having different views) and then converge (eventually become consistent):
 * - lower latency (does not need to wait for all replicas to agree)
 * - higher availability (can still work even if some replicas are down or partitioned)
 *
 * Rules for GCCounter:
 * - a counter can only increment it's own count
 * - a counter can only increment count of machine that it knows about
 * - when reconciling two counters, the count for each machine should be the maximum of the two counts but not the sum
 * - the total count is the sum of all machine counts
 *
 * Some observations about GCCounter:
 * - Addition is used for increment and total
 * - Maximum used in merge
 * - identity element 0 is used in both cases above
 * --> monoids must be used here somewhere
 *
 * For incrementing we need associativity and identity -> monoid
 * For total we need:
 * - associativity and commutativity so order of sum does not matter
 * - identity element 0 for machine with no count
 * -> commutative monoid
 *
 * For merging we need:
 * - communicative property is important to make sure that merge order does not matter
 * - associative property ensure merge of more than two counters yield the correct result
 * - idempotent property ensure merging of same counter always yield the same result
 * -> idempotent commutative monoid (bounded semilattice)
 * */
final case class GCCounter1(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCCounter1 = {
    val newCount = counters.getOrElse(machine, 0) + amount
    GCCounter1(counters + (machine -> newCount))
  }

  def merge(that: GCCounter1): GCCounter1 = {
    val machines = counters.keySet ++ that.counters.keySet
    val merged = machines.map { machine =>
      machine -> Math.max(counters.getOrElse(machine, 0), that.counters.getOrElse(machine, 0))
    }.toMap
    GCCounter1(merged)
  }

  def total: Int = counters.values.sum
}

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intMaxBoundedSemiLattice = new BoundedSemiLattice[Int] {
    def combine(a1: Int, a2: Int): Int = a1 max a2
    def empty: Int = 0
  }

  implicit def setBoundedSemiLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
    def combine(a1: Set[A], a2: Set[A]): Set[A] = a1 ++ a2
    def empty: Set[A] = Set.empty[A]
  }
}

final case class GCCounter2[A](counters: Map[String, A]) {
    def increment(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCCounter2[A] = {
      val newCount = counters.getOrElse(machine, m.empty) |+| amount
      GCCounter2(counters + (machine -> newCount))
    }

    def merge(that: GCCounter2[A])(implicit b: BoundedSemiLattice[A]): GCCounter2[A] = {
      val merged = counters |+| that.counters
      GCCounter2(merged)
    }

    def total(implicit m: CommutativeMonoid[A]): A = counters.values.toList.combineAll // same as foldLeft(m.empty)(_ |+| _)
}

trait GCounter[F[_, _], K, V] {
  def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]
  def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
  def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
}

object GCounter {
  def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
    def increment(f: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
      val newV = f.getOrElse(k, m.empty) |+| v
      f + (k -> newV)
    }

    def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2

    def total(f: Map[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.toList.combineAll
  }

  implicit def gCounterKvs[F[_, _], K, V](implicit kvs: KeyValueStore[F], km: CommutativeMonoid[F[K, V]]): GCounter[F, K, V] = new GCounter[F, K, V] {
    def increment(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V] = {
      val newV = f.getOrElse(k, m.empty) |+| v
      kvs.put(f)(k, newV)
    }

    def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

    def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V = f.values.combineAll
  }
}

// Custom keyValueStore implementation
trait KeyValueStore[F[_, _]] {
  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
  def get[K, V](f: F[K, V])(k: K): Option[V]
  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)
  def values[K, V](f: F[K, V]): List[V]
}

object KeyValueStore {
  def mapKeyValueStore: KeyValueStore[Map] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)
    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] = kvs.put(f)(key, value)
    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)
    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V = kvs.getOrElse(f)(key, default)
    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }
}