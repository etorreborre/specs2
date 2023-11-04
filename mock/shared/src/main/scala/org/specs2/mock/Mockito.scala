package org.specs2
package mock

import mockito._
import matcher.Expectations
import matcher.ThrownExpectations

/**
 * This trait can be used to access Mockito functionalities.
 */
trait Mockito extends MocksCreation
  with CalledMatchers
  with MockitoStubs
  with CapturedArgument
  with MockitoMatchers
  with ArgThat
  with Expectations
  with MockitoFunctions

object Mockito extends Mockito

// This object provides mockito expectations which will throw an exception in case of a failure.
// It should be used with either org.specs2.mutable.Specification or org.specs2.SpecificationWithJUnit
// which are specifications throwing exceptions when there are failures
object MockitoThrownExpectations extends Mockito with ThrownExpectations
