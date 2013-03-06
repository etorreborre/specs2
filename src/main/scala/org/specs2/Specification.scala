package org.specs2

import control._
import main.{ArgumentsShortcuts, ArgumentsArgs}
import time._
import execute._
import matcher._
import specification._
import Functions._

/**
 * The Specification trait can be extended to create a specification.
 *
 * It provides:
 *
 *  - methods to create specification fragments
 *  - methods to specify arguments (for execution and reporting)
 *  - matchers
 *
 *  @see SpecificationFeatures for more details
 */
abstract class Specification extends SpecificationLike
trait SpecificationLike extends SpecificationStructure with SpecificationFeatures

/**
 * The SpecificationFeatures trait provides functionalities to build the specification fragments plus additional ones:
 *
 *  - ArgumentArgs: `args` method to create Arguments
 *  - MustMatchers: methods for creating expectations with the `must` verb like `a must_== b`
 *  - ShouldMatchers: methods for creating expectations with the `should` verb like `a should_== b`
 *  - FormattingFragments: to use `p`, `br`, `end` to format the specification text
 *  - StandardResults: to use todo, pending, success in example bodies
 *  - AutoExamples: to allow the declaration of examples where the body of the example is its own description
 *  - TimeConversions: to create time expressions like `1.second`
 *  - PendingUntilFixed: to mark an example pending until it succeeds
 *  - Contexts: implicit methods to apply contexts to Results
 *  - Debug: to allow the insertion of .pp to print any expression to the Console and pass
 *           it to the rest of the program
 */
trait SpecificationFeatures extends FragmentsBuilder with SpecificationStringContext
   with AutoExamples
   with SpecificationInclusion
   with ArgumentsArgs
   with ArgumentsShortcuts
   with MustMatchers
   with ShouldMatchers
   with FormattingFragments
   with StandardResults
   with StandardMatchResults
   with TimeConversions
   with PendingUntilFixed
   with Contexts
   with SpecificationNavigation
   with Debug {

  /**
   * apply an implicit context to a MatchResult in order to allow the implicit passing of a context to each example
   *
   * @see examples.DefineContextsSpec#BeforeWithImplicitContextSpec
   */
  implicit def contextAsResult[T, M[_] <: MatchResult[_]](implicit context: Context = defaultContext): AsResult[M[T]] = new AsResult[M[T]] {
    def asResult(t: =>M[T]) = context(t.toResult)
  }

  /**
   * apply an implicit Outside context to a function returning anything convertible to a result
   *
   * @see examples.DefineContextsSpec#OutsideWithImplicitContextSpec
   */
  /** use an available outside context to transform a function returning a value convertible to a result, into a result */
  implicit def outsideFunctionToResult[T : Outside, R : AsResult]: AsResult[T => R] = new AsResult[T => R] {
    def asResult(f: =>(T => R)) = implicitly[Outside[T]].apply((t: T) => AsResult(f(t)))
  }
}
