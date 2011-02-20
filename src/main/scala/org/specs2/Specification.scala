package org.specs2

import control._
import time._
import main.ArgumentsArgs
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
 *  * Debug: to allow the insertion of .pp to print any expression to the Console and pass
 *           it to the rest of the program
 */
trait SpecificationFeatures extends FragmentsBuilder
   with SpecificationInclusion
   with ArgumentsArgs
   with MustMatchers
   with ShouldMatchers
   with FormattingFragments
   with StandardResults
   with StandardMatchResults
   with AutoExamples
   with TimeConversions
   with PendingUntilFixed
   with Debug
