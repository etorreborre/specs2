package org.specs2
package mock
package mockito

import org.mockito.Matchers

trait MockitoMatchers {
  def anyString = Matchers.anyString
  def any[T : ClassManifest]: T = org.mockito.Matchers.isA(implicitly[ClassManifest[T]].erasure).asInstanceOf[T]
}