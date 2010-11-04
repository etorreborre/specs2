package org.specs2
package mock
package mockito

import org.mockito.ArgumentCaptor

/**
 * Syntactic sugar on top of the ArgumentCaptor API
 * to avoid using classOf and an explicit call to capture()
 */
trait CapturedArgument {
  /** capture an argument of type T */
  def capture[T : ClassManifest]: ArgumentCapture[T] = new ArgumentCapture[T]

  /** This class encapsulates an ArgumentCaptor */
  class ArgumentCapture[T](implicit m: ClassManifest[T]) {
    lazy private val captor: ArgumentCaptor[T] = ArgumentCaptor.forClass(m.erasure).asInstanceOf[ArgumentCaptor[T]]
    def value = captor.getValue
    def capture = captor.capture()
  }
  /** this conversion allows to capture the parameter is a mocked call */
  implicit def captured[T](c: ArgumentCapture[T]): T = c.capture
}
