package org.specs2
package specification
package dsl
package mutable

import core.{SpecificationStructure, SpecStructure}

/**
 * Dsl for creating links in a mutable specification
 */
trait LinkDsl extends FragmentBuilder with dsl.LinkDsl {

  implicit class mutableLinkFragment(alias: String) extends linkFragment(alias) {
    override def ~(s: SpecStructure)                              = addFragment(super.~(s))
    override def ~(s: SpecStructure, tooltip: String)             = addFragment(super.~(s, tooltip))
    override def ~(s: => SpecificationStructure)                  = addFragment(super.~(s))
    override def ~(s: => SpecificationStructure, tooltip: String) = addFragment(super.~(s, tooltip))
  }

  implicit class mutableSeeFragment(alias: String) extends seeFragment(alias) {
    override def ~/(s: SpecStructure)                             = addFragment(super.~/(s))
    override def ~/(s: SpecStructure, tooltip: String)            = addFragment(super.~/(s, tooltip))
    override def ~/(s: =>SpecificationStructure)                  = addFragment(super.~/(s))
    override def ~/(s: =>SpecificationStructure, tooltip: String) = addFragment(super.~/(s, tooltip))
  }

  override def link(s: SpecStructure)            = addFragment(super.link(s))
  override def link(s: =>SpecificationStructure) = addFragment(super.link(s))
  override def see(s: SpecStructure)             = addFragment(super.see(s))
  override def see(s: =>SpecificationStructure)  = addFragment(super.see(s))

}
