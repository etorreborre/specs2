package org.specs2
package specification
package dsl

import core.SpecHeader
import scala.implicits.Not

/**
 * Dsl for creating a specification title
 */
trait TitleDsl { outer =>

  def title(s: String): SpecHeader =
    extension_title(s)

  extension (s: String)(using not: Not[NoTitleDsl])
    def title: SpecHeader = SpecHeader(outer.getClass, Some(s))

}

/** deactivate the TitleDsl implicits */
trait NoTitleDsl extends TitleDsl:
  given NoTitleDsl = ???
