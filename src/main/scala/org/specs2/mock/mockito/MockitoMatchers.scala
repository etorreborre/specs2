package org.specs2
package mock
package mockito

import org.mockito.Matchers
import org.hamcrest.core.IsAnything

/**
 * Mockito Matchers for the most common types
 */
trait MockitoMatchers extends ArgThat {

  def anyString = Matchers.anyString
  def anyByte   = Matchers.anyByte
  def anyShort  = Matchers.anyShort
  def anyChar   = Matchers.anyChar
  def anyInt    = Matchers.anyInt
  def anyLong   = Matchers.anyLong
  def anyDouble = Matchers.anyDouble
  def anyFloat  = Matchers.anyFloat

  def any[T : Manifest]: T = org.mockito.Matchers.any(implicitly[Manifest[T]].erasure).asInstanceOf[T]

  def anyFunction1[A, R]                      = anArgThat(new IsAnything[Function1[A, R]])
  def anyFunction2[A, B, R]                   = anArgThat(new IsAnything[Function2[A, B, R]])
  def anyFunction3[A, B, C, R]                = anArgThat(new IsAnything[Function3[A, B, C, R]])
  def anyFunction4[A, B, C, D, R]             = anArgThat(new IsAnything[Function4[A, B, C, D, R]])
  def anyFunction5[A, B, C, D, E, R]          = anArgThat(new IsAnything[Function5[A, B, C, D, E, R]])
  def anyFunction6[A, B, C, D, E, F, R]       = anArgThat(new IsAnything[Function6[A, B, C, D, E, F, R]])
  def anyFunction7[A, B, C, D, E, F, G, R]    = anArgThat(new IsAnything[Function7[A, B, C, D, E, F, G, R]])
  def anyFunction8[A, B, C, D, E, F, G, H, R] = anArgThat(new IsAnything[Function8[A, B, C, D, E, F, G, H, R]])

}