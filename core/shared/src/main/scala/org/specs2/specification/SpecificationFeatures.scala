package org.specs2
package specification

import matcher.*
import execute.*
import control.Debug
import concurrent.ImplicitExecutionContexts

/** List of all the features which are being included the default Specification class:
  *
  *   - matchers (with the expectations dsl and the most common matchers)
  *   - standard results
  *   - pending until fixed
  *   - implicit parameters to overload some method calls
  *   - .pp calls to print out some expressions
  */
trait SpecificationFeatures
    extends MustMatchers
    with ShouldMatchers
    with Expectations
    with StandardResults
    with ExpectedResults
    with MatcherImplicits
    with ResultLogicalCombinators
    with PendingUntilFixed
    with Debug
    with ImplicitExecutionContexts
