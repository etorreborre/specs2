package org.specs2
package specification
package dsl
package mutable

import org.specs2.specification.core.{SpecificationStructure, SpecStructure}
import control.ImplicitParameters.ImplicitParam

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

  override def link(s: SpecStructure)  = addFragment(super.link(s))
  override def see(s: SpecStructure) = addFragment(super.see(s))
  override def see(s: =>SpecificationStructure)(implicit p: ImplicitParam) = addFragment(super.see(s)(p))

}
