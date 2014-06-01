package org.specs2
package specification
package dsl
package mutable

import specification.dsl
import specification.core.SpecStructure

/**
 * DSL for adding links to other specifications
 */
trait LinkDsl extends FragmentBuilder with dsl.LinkDsl {

  implicit class mutableLinkFragment(text: String) extends linkFragment(text) {
    override def ~(s: SpecStructure)                = addFragment(super.~(s))
    override def ~(s: SpecStructure, after: String) = addFragment(super.~(s, after))
    override def ~(alias: String, s: SpecStructure, after: String, tooltip: String) = addFragment(super.~(alias, s, after, tooltip))
    override def ~(s: SpecStructure, after: String, tooltip: String) = addFragment(super.~(s, after, tooltip))
  }

  override def link(s: SpecStructure) = addFragment(super.link(s))

}
