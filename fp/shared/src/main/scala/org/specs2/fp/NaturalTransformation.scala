package org.specs2.fp

trait NaturalTransformation[-F[_], +G[_]]:
  def apply[A](fa: F[A]): G[A]

object NaturalTransformation:

  given naturalId[M[_] : Monad] as NaturalTransformation[Id, M] = new NaturalTransformation[Id, M]:
    def apply[A](fa: Id[A]): M[A] =
      summon[Monad[M]].point(fa)
  