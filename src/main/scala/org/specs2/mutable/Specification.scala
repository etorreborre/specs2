package org.specs2
package mutable

import control._
import time._
import execute._
import matcher._
import main.ArgumentsShortcuts
import specification._

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
   with SpecificationNavigation
   with Debug {

  /** transform a context to a result to allow the implicit passing of a context to each example */
  implicit def contextToResult[T](t: MatchResult[T])(implicit context: Context = defaultContext): Result = context(asResult(t))
  /** use an available outside context to transform a function returning a value convertible to a result, into a result */
  implicit def outsideFunctionToResult[T, R](implicit outside: Outside[T], conv: R => Result) : (T => R) => Result = (f: T => R) => outside((t: T) => conv(f(t)))

}
