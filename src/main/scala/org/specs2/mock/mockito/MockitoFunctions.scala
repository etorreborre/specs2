package org.specs2
package mock
package mockito

import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock

/**
 * shortcuts to standard Mockito functions
 */
trait MockitoFunctions extends TheMockitoMocker {
    /** delegate to MockitoMocker doReturn. */
  def doReturn[T](t: T) = mocker.doReturn(t)
  /** delegate to MockitoMocker doAnswer. */
  def doAnswer[T](a: Answer[T]) = mocker.doAnswer(a)
  /** delegate to MockitoMocker doThrow. */
  def doThrow[E <: Throwable](e: E) = mocker.doThrow(e)
  /** delegate to MockitoMocker doNothing. */
  def doNothing = mocker.doNothing
}
