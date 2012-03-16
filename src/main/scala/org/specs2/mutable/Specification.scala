package org.specs2
package mutable

import control._
import time._
import execute._
import matcher._
import main.ArgumentsShortcuts
import specification.{SpecificationStructure, Contexts, Context, Outside}

trait Specification extends SpecificationStructure with SpecificationFeatures {
  def is = specFragments
}

trait SpecificationFeatures extends FragmentsBuilder
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
   with Debug {
  /** transform a context to a result to allow the implicit passing of a context to each example */
  implicit def contextToResult[T](t: MatchResult[T])(implicit context: Context = defaultContext): Result = context(asResult(t))
  /** use an available outside context to transform a function returning a MatchResult into a result */
  implicit def outsideFunctionToResult[T, S](implicit o: Outside[T]) = (f: T => MatchResult[S]) => { o((t1:T) => f(t1).toResult) }

}
