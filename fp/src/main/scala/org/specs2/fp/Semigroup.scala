package org.specs2.fp

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Semigroup[F] {

  def append(f1: F, f2: => F): F

  def multiply1(value: F, n: Int): F = {
    @scala.annotation.tailrec
    def go(x: F, y: Int, z: F): F = y match {
      case y if (y & 1) == 0 => go(append(x, x), y >>> 1, z)
      case y if (y == 1)     => append(x, z)
      case _                 => go(append(x, x), (y - 1) >>>  1, append(x, z))
    }
    if (n <= 0) value else go(value, n, value)
  }

}

object Semigroup {
  @inline def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  /** Make an associative binary function into an instance. */
  def instance[A](f: (A, => A) => A): Semigroup[A] =
    new Semigroup[A] {
      def append(f1: A, f2: => A): A = f(f1,f2)
    }

  /** A purely left-biased semigroup. */
  /** `point(a) append (point(a) append (point(a)...` */
  def repeat[F[_], A](a: A)(implicit F: Applicative[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), repeat[F, A](a))

  /** `point(a) append (point(f(a)) append (point(f(f(a)))...` */
  def iterate[F[_], A](a: A)(f: A => A)(implicit F: Applicative[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), iterate[F, A](f(a))(f))

}

trait SemigroupSyntax {

  implicit class SemigroupOps[M : Semigroup](a: M) {
    def append(b: =>M): M =
      Semigroup.apply[M].append(a, b)

    def |+|(b: =>M): M =
      append(b)
  }

}
