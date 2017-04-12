package org.specs2
package specification

import matcher.{StandardMatchResults, ShouldMatchers, MustMatchers}
import execute.{PendingUntilFixed, StandardResults}
import control.Debug
import control.ImplicitParameters
import org.specs2.concurrent.ImplicitExecutionContexts

/**
 * List of all the features which are being included the default Specification class:
 *
 *  - matchers (with the expectations dsl and the most common matchers)
 *  - standard results
 *  - pending until fixed
 *  - implicit parameters to overload some method calls
 *  - .pp calls to print out some expressions
 *
 */
trait SpecificationFeatures extends
       MustMatchers
  with ShouldMatchers
  with StandardResults
  with StandardMatchResults
  with PendingUntilFixed
  with ImplicitParameters
  with Debug
  with ImplicitExecutionContexts

