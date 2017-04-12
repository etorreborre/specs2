package org.specs2
package mock
package mockito

import org.mockito.ArgumentCaptor
import scala.reflect.ClassTag

/**
 * Syntactic sugar on top of the ArgumentCaptor API
 * to avoid using classOf and an explicit call to capture()
 */
trait CapturedArgument {
  /** capture an argument of type T */
  def capture[T : ClassTag]: ArgumentCapture[T] = new ArgumentCapture[T]

  /** this conversion allows to capture the parameter is a mocked call */
  implicit def captured[T](c: ArgumentCapture[T]): T = c.capture
}

object CapturedArgument extends CapturedArgument

/** This class encapsulates an ArgumentCaptor */
class ArgumentCapture[T](implicit m: ClassTag[T]) {
  lazy private val captor: ArgumentCaptor[T] = ArgumentCaptor.forClass(m.runtimeClass).asInstanceOf[ArgumentCaptor[T]]
  def value = captor.getValue
  def values = captor.getAllValues
  def capture = captor.capture()
}

