package org.specs2
package specification
package dsl
package mutable

import specification.core.SpecHeader
import scala.util.Not

/**
 * Dsl for creating a title in a mutable specification
 */
trait TitleDsl extends MutableHeaderBuilder with specification.dsl.TitleDsl:

  extension (s: String)(using not: Not[NoTitleDsl])
    override def title: SpecHeader = setTitle(s)
