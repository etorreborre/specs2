package org.specs2
package mock
package mockito

import org.mockito.Matchers

trait MockitoMatchers {
  def anyString = Matchers.anyString
  def anyByte   = Matchers.anyByte
  def anyShort  = Matchers.anyShort
  def anyChar   = Matchers.anyChar
  def anyInt    = Matchers.anyInt
  def anyLong   = Matchers.anyLong
  def anyDouble = Matchers.anyDouble
  def anyFloat  = Matchers.anyFloat

  def any[T : Manifest]: T = org.mockito.Matchers.any(implicitly[Manifest[T]].erasure).asInstanceOf[T]
}