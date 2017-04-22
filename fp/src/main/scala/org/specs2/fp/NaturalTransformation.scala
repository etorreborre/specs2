package org.specs2.fp

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object NaturalTransformation {

  implicit def naturalId[M[_] : Monad]: NaturalTransformation[Id, M] = new NaturalTransformation[Id, M] {
    def apply[A](fa: Id[A]): M[A] =
      Monad[M].point(fa)
  }

}
