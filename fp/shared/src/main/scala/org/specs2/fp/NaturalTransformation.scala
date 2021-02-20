package org.specs2.fp

trait NaturalTransformation[-F[_], +G[_]]:
  def apply[A](fa: F[A]): G[A]

object NaturalTransformation:

  given naturalId[M[_] : Monad]: NaturalTransformation[Id, M] with
    def apply[A](fa: Id[A]): M[A] =
      summon[Monad[M]].point(fa)
