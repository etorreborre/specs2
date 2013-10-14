package org.specs2
package specification

import matcher.ResultMatchers
import main.Arguments

/**
 * execute a Specification and check the results
 */
trait SpecificationExecution extends ResultMatchers { this: Specification =>
  def executeSpec(s: SpecificationStructure)       = FragmentExecution.executeExamplesResult(s.content)(Arguments())
  def executionMustBeOk(s: SpecificationStructure) = executeSpec(s) must beSuccessful
}