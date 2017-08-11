package org.specs2

/**
 * Code in the fp package is explicitly inspired from the Scalaz and cats projects
 *
 * Please refer to http://github.com/scalaz/scalaz and http://github.com/typelevel/cats for more information
 */
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

  type Id[X] = X

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
}
