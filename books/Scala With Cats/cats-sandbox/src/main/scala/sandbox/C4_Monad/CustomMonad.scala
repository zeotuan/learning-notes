package sandbox.C4_Monad

import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.instances.option._
import cats.Monad
import cats.implicits.catsSyntaxMonadIdOps

import scala.annotation.tailrec

object CatCustomMonad {
  implicit val optionMonad = new Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    override def pure[A](x: A): Option[A] = Option(x)

    // tailRecM is a helper function that allows us to implement flatMap in a stack-safe way
    // https: //functorial.com/stack-safety-for-free/index.pdf
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(a1)) => tailRecM(a1)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  def unsafeRetry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] = {
    f(start).flatMap(unsafeRetry(_)(f))
  }

  def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] = {
    Monad[F].tailRecM(start) { a => f(a).map(b => Left(b)) }
  }

  def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] = start.iterateWhileM(f)(_ => true)
}

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]


object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
      case Leaf(value) => f(value)
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] = open match {
        case Branch(l, r) :: next => loop(l :: r :: next, closed)
        case Leaf(Left(a1)) :: next => loop(f(a1) :: next, closed)
        case Leaf(Right(b)) :: next => loop(next, Some(pure(b)) :: closed)
        case Nil => closed.foldLeft(Nil: List[Tree[B]])((acc, mTree) =>
          mTree.map(_ :: acc).getOrElse {
            val left :: right :: tail = acc
            branch(left, right) :: tail
          }
        )
      }
      loop(List(f(a)), Nil).head
    }
  }
}

