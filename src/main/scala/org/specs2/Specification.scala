package org.specs2

import control._
import main.{ArgumentsShortcuts, ArgumentsArgs}
import time._
import execute._
import matcher._
import specification._

/**
 * The Specification trait holds all the Specification fragments + provides matching and other features
 */
trait Specification extends SpecificationStructure with SpecificationFeatures

/**
 * The SpecificationFeatures trait provides functionalities to build the specification fragments plus additional ones:
 *  * ArgumentArgs: arg method to create Arguments
 *  * MustMatchers: methods for creating expectations with the `must` verb like `a must_== b`
 *  * ShouldMatchers: methods for creating expectations with the `should` verb like `a should_== b`
 *  * FormattingFragments: to use p, br, end to format the specification text
 *  * StandardResults: to use todo, pending, success in example bodies
 *  * AutoExamples: to allow the declaration use todo, pending, success in example bodies
 *  * TimeConversions: to create time expressions like 1.second
 *  * PendingUntilFixed: to mark an example pending until it succeeds
 *  * Contexts: context methods for results
 *  * Debug: to allow the insertion of .pp to print any expression to the Console and pass
 *           it to the rest of the program
 */
trait SpecificationFeatures extends FragmentsBuilder
   with SpecificationInclusion
   with ArgumentsArgs
   with ArgumentsShortcuts
   with MustMatchers
   with ShouldMatchers
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
