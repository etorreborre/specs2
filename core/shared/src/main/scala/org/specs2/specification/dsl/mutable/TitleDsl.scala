package org.specs2
package specification
package dsl
package mutable

import specification.core.SpecHeader
import scala.implicits.Not

/**
 * Dsl for creating a title in a mutable specification
 */
trait TitleDsl extends MutableHeaderBuilder with specification.dsl.TitleDsl {

  override def title(s: String): SpecHeader =
    extension_title(s)

  extension (s: String)(using not: Not[NoTitleDsl])
    override def title: SpecHeader = setTitle(s)
}
