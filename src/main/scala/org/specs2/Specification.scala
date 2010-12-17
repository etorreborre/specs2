package org.specs2

import control._
import time._
import main.ArgumentsArgs
import execute._
import matcher._
import specification._

/**
 * The Specification trait provides BaseSpecification functionalities plus additional ones:
 *  * ArgumentArgs: arg method to create Arguments 
 *  * MustMatchers: methods for creating expectations with the `must` verb like `a must_== b` 
 *  * ShouldMatchers: methods for creating expectations with the `should` verb like `a should_== b`
 *  * PredefinedFragments: to use p, br, end to format the specification text
 *  * StandardResults: to use todo, pending, success in example bodies
 *  * AutoExamples: to allow the declaration use todo, pending, success in example bodies
 *  * Debug: to allow the insertion of .pp to print any expression to the Console and pass
 *           it to the rest of the program
 */
trait Specification extends BaseSpecification 
   with ArgumentsArgs
   with MustMatchers
   with ShouldMatchers
   with PredefinedFragments
   with StandardResults
   with StandardMatchResults
   with AutoExamples
   with TimeConversions
   with Debug
