package org.specs2.fp

import syntax._

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {

  /** Transform `fa` using `f`, collecting all the `G`s with `ap`. */
  def traverseImpl[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  class Traversal[G[_]](implicit G: Applicative[G]) {
    def run[A,B](fa: F[A])(f: A => G[B]): G[F[B]] = traverseImpl[G,A,B](fa)(f)
  }

  def traversal[G[_]:Applicative]: Traversal[G] =
    new Traversal[G]

  def traverse[G[_]: Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    traversal[G].run(fa)(f)

  final def traverseM[A, G[_], B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: Monad[F]): G[F[B]] =
    G.map(G.traverse(fa)(f)(this))(F.join)

  def flatTraverse[G[_], A, B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: Monad[F]): G[F[B]] =
    G.map(traverse(fa)(f))(F.join)

  /** Traverse with the identity function. */
  def sequence[G[_]: Applicative,A](fga: F[G[A]]): G[F[A]] =
    traversal[G].run[G[A], A](fga)(ga => ga)

}

object Traverse {
  @inline def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  implicit val listInstance: Traverse[List] = new Traverse[List] {
    def traverseImpl[G[_]: Applicative, A, B](fa: List[A])(f: A => G[B]): G[List[B]] = {
      val g = Applicative.apply[G]
      fa match {
        case Nil => g.point(Nil)
        case h :: t => g.apply2(f(h), traverseImpl(t)(f))((b, r) => b :: r)
      }
    }

    def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)

    def foldMap[A, B](fa: List[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa.foldLeft(F.zero)((res, cur) => res |+| f(cur))

    def foldRight[A, B](fa: List[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((cur, res) => f(cur, res))

    def foldLeft[A, B](fa: List[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)

  }

  implicit val vectorInstance: Traverse[Vector] = new Traverse[Vector] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Vector[A])(f: A => G[B]): G[Vector[B]] = {
      val g = Applicative.apply[G]
      fa match {
        case h +: t => g.apply2(f(h), traverseImpl(t)(f))((b, r) => b +: r)
        case _ => g.point(Vector.empty)
      }
    }

    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] =
      fa.map(f)

    def foldMap[A, B](fa: Vector[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa.foldLeft(F.zero)((res, cur) => res |+| f(cur))

    def foldRight[A, B](fa: Vector[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((cur, res) => f(cur, res))

    def foldLeft[A, B](fa: Vector[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)
  }

  implicit def optionInstance[L]: Traverse[Option] = new Traverse[Option] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = {
      val g = Applicative.apply[G]
      fa match {
        case None    => g.point(None)
        case Some(a) => g.map(f(a))(Some.apply)
      }
    }

    def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)

    def foldMap[A, B](fa: Option[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa.foldLeft(F.zero)((res, cur) => res |+| f(cur))

    def foldRight[A, B](fa: Option[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((cur, res) => f(cur, res))

    def foldLeft[A, B](fa: Option[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)

  }

  implicit def eitherInstance[L]: Traverse[Either[L, ?]] = new Traverse[Either[L, ?]] {
    def traverseImpl[G[_]: Applicative, A, B](fa: Either[L, A])(f: A => G[B]): G[Either[L, B]] = {
      val g = Applicative.apply[G]
      fa match {
        case Left(l)  => g.point(Left(l))
        case Right(a) => g.map(f(a))(Right.apply)
      }
    }

    def map[A, B](fa: Either[L, A])(f: A => B): Either[L, B] =
      fa match {
        case Left(l)  => Left(l)
        case Right(a) => Right(f(a))
      }

    def foldMap[A, B](fa: Either[L, A])(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Left(l) => F.zero
        case Right(a) => f(a)
      }

    def foldRight[A, B](fa: Either[L, A], z: => B)(f: (A, => B) => B): B =
      fa match {
        case Left(l) => z
        case Right(a) => f(a, z)
      }

    def foldLeft[A, B](fa: Either[L, A], z: B)(f: (B, A) => B): B =
      fa match {
        case Left(l) => z
        case Right(a) => f(z, a)
      }
  }
}


trait TraverseSyntax {

  implicit class TraverseOps[F[_] : Traverse, A](fa: F[A]) {
    def traverse[G[_] : Applicative, B](f: A => G[B]): G[F[B]] =
      Traverse.apply[F].traverse(fa)(f)
  }

  implicit class SequenceOps[F[_] : Traverse, G[_] : Applicative, A](fa: F[G[A]]) {
    def sequence: G[F[A]] =
      Traverse.apply[F].sequence(fa)
  }
}

object TraverseSyntax extends TraverseSyntax
