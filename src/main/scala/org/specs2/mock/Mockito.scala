package org.specs2
package mock

import mockito._

/**
 * This trait can be used to access Mockito functionalities.
 */
trait Mockito extends MocksCreation
  with CalledMatchers
  with MockitoStubs 
  with CapturedArgument 
  with MockitoMatchers

