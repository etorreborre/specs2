package org.specs2

package object fp {

  object syntax extends
       FunctorSyntax
  with ApplicativeSyntax
  with MonadSyntax
  with TraverseSyntax
  with FoldableSyntax
  with SemigroupSyntax
  with ShowSyntax
  with EitherSyntax

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
}
