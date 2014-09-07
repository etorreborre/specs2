package org.specs2
package specification
package dsl
package mutable

import specification.core.SpecHeader

/**
 * Dsl for creating a title in a mutable specification
 */
trait TitleDsl extends MutableHeaderBuilder with specification.dsl.TitleDsl {
  override implicit def title(s: String) = new MutableTitleOps(s)
  class MutableTitleOps(s: String) extends TitleOps(s) {
    override def title: SpecHeader = setTitle(s)
  }
}