package org.specs2
package specification
package dsl

import core.SpecHeader
import scala.util.NotGiven

/**
 * Dsl for creating a specification title
 */
trait TitleDsl:
  outer =>

  def title(s: String)(using nothing: Int = 0): SpecHeader =
    s.title

  extension (s: String)(using not: NotGiven[NoTitleDsl])
    def title: SpecHeader = SpecHeader(outer.getClass, Some(s))


/** deactivate the TitleDsl implicits */
trait NoTitleDsl extends TitleDsl:
  given NoTitleDsl = ???
