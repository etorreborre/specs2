package org.specs2
package specification
package dsl

import core.SpecHeader

/**
 * Dsl for creating a specification title
 */
trait TitleDsl { outer =>

  implicit def title(s: String) = new TitleOps(s)
  class TitleOps(s: String) {
    def title: SpecHeader = SpecHeader(outer.getClass, Some(s))
  }

}

