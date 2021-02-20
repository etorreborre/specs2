package org.specs2.specification.create

import org.specs2.specification.core.*

/**
 * An interpolated element of a S2 string.
 *
 * It takes the current piece of text preceding it to create Fragments
 * to be appended to the already created Fragments
 *
 * For example if the interpolated element is an execution then prepend will take the text
 * to create an Example
 */
trait Interpolated:
  def prepend(text: String): Fragments
