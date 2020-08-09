package org.specs2.specification.create

import org.specs2.specification.core._

/**
 * An interpolated element of a S2 string.
 *
 * It takes the current piece of text preceding it to create Fragments
 * to be appended to the already created Fragments
 *
 * For example if the interpolated element is a String and the last created fragment is a Text,
 * then we can modify that fragment to append the `text`
 */
trait Interpolated {
  def append(text: String): Fragments
}
