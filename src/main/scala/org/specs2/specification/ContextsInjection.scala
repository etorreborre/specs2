package org.specs2
package specification

import matcher.MatchResult
import execute.AsResult

trait ContextsInjection extends Contexts {

  /** transform a context to a result to allow the implicit passing of a context to each example */
  implicit def contextAsResult[T, M[_] <: MatchResult[_]](implicit context: Context = defaultContext): AsResult[M[T]] = new AsResult[M[T]] {
    def asResult(t: =>M[T]) = context(t.toResult)
  }
  /** use an available outside context to transform a function returning a value convertible to a result, into a result */
  implicit def outsideFunctionToResult[T : Outside, R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = AsResult(implicitly[Outside[T]].applyOutside(f))
  }
  /** apply an implicit Fixture */
  implicit def fixtureFunctionToResult[T : Fixture, R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = implicitly[Fixture[T]].apply(f)
  }

}

