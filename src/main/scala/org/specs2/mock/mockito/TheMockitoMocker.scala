package org.specs2
package mock
package mockito


/** delegate to Mockito static methods with appropriate type inference. */
trait TheMockitoMocker {
  private[specs2] val mocker = new MockitoMocker {}
}