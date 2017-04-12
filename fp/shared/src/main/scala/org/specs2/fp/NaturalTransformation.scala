package org.specs2.fp

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}
