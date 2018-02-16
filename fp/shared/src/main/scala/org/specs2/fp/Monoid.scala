package org.specs2.fp

/**
 * Inspired from the scalaz (https://github.com/scalaz/scalaz) project
 */
trait Monoid[F] extends Semigroup[F] {

  def zero: F

  def multiply(value: F, n: Int): F =
    if (n <= 0) zero else multiply1(value, n - 1)
}

object Monoid {
  @inline def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  /** Make an append and zero into an instance. */
  def instance[A](f: (A, => A) => A, z: A): Monoid[A] =
    new Monoid[A] {
      def zero = z
      def append(f1: A, f2: => A): A = f(f1,f2)
    }

  implicit val intMonoid: Monoid[Int] =
    instance((s1, s2) => s1 + s2, 0)

  implicit def listMonoid[A]: Monoid[List[A]] =
    instance((s1, s2) => s1 ++ s2, List.empty[A])

  implicit def vectorMonoid[A]: Monoid[Vector[A]] =
    instance((s1, s2) => s1 ++ s2, Vector.empty[A])

  implicit val stringMonoid: Monoid[String] =
    instance((s1, s2) => s1 + s2, "")

  implicit def streamMonoid[A]: Monoid[Stream[A]] =
    instance((s1, s2) => s1 ++ s2, Stream.empty[A])

  implicit def mapMonoid[K, V : Monoid]: Monoid[Map[K, V]] = {
    def merge(m1: Map[K, V], m2: Map[K, V]): Map[K, V] =
      m2.foldLeft(m1) { case (res, (k, v)) => res.updated(k, res.get(k).map(Monoid[V].append(_, v)).getOrElse(v)) }

    instance((s1, s2) => merge(s1, s2), Map())
  }


}

trait MonoidSyntax {

  implicit class MonoidOps[A : Monoid](a1: A) {
    def |+|(a2: A): A =
      Monoid[A].append(a1, a2)
  }

}
