package org.specs2
package mutable

import control._
import time._
import execute._
import matcher._
import main.ArgumentsShortcuts
import specification._
import control.Functions._

abstract class Specification extends SpecificationLike
trait SpecificationLike extends SpecificationStructure with SpecificationFeatures {
  def is = fragments
}

trait SpecificationFeatures extends mutable.FragmentsBuilder
   with SpecificationStringContext
   with mutable.SpecificationInclusion
   with ArgumentsArgs
   with ArgumentsShortcuts
   with MustThrownMatchers
   with ShouldThrownMatchers
   with FormattingFragments
   with StandardResults
   with StandardMatchResults
   with AutoExamples
   with TimeConversions
   with PendingUntilFixed
   with Contexts
   with SpecificationNavigation
   with Debug {

  /** transform a context to a result to allow the implicit passing of a context to each example */
  implicit def contextAsResult[T, M[_] <: MatchResult[_]](implicit context: Context = defaultContext): AsResult[M[T]] = new AsResult[M[T]] {
    def asResult(t: =>M[T]) = context(t.toResult)
  }
  /** use an available outside context to transform a function returning a value convertible to a result, into a result */
  implicit def outsideFunctionToResult[T : Outside, R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = implicitly[Outside[T]].applyOutside(f)
  }
}
