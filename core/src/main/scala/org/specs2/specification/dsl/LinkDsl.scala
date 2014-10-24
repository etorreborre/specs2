package org.specs2
package specification
package dsl

import core._
import create.FragmentsFactory

/**
 * Dsl for creating links
 */
trait LinkDsl extends LinkCreation {

  implicit class linkFragment(alias: String) {
    def ~(s: SpecStructure): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, alias = alias))

    def ~(s: SpecStructure, tooltip: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, alias = alias, tooltip = tooltip))

    def ~(s: => SpecificationStructure): Fragment =
      fragmentFactory.link(SpecificationLink(s.is.header, alias = alias))

    def ~(s: => SpecificationStructure, tooltip: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.is.header, alias = alias, tooltip = tooltip))
  }

  implicit class seeFragment(alias: String) {
    def ~/(s: SpecStructure): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, alias = alias))

    def ~/(s: SpecStructure, tooltip: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, alias = alias, tooltip = tooltip))

    def ~/(s: => SpecificationStructure): Fragment =
      fragmentFactory.see(SpecificationLink(s.is.header, alias = alias))

    def ~/(s: => SpecificationStructure, tooltip: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.is.header, alias = alias, tooltip = tooltip))
  }

}

/**
 * Create links without any implicits
 */
trait LinkCreation extends FragmentsFactory {
  def link(s: SpecStructure): Fragment            = fragmentFactory.link(SpecificationLink(s.header, alias = s.header.showWords))
  def link(s: =>SpecificationStructure): Fragment = fragmentFactory.link(SpecificationLink(s.is.header, alias = s.is.header.showWords))

  def see(s: SpecStructure): Fragment            = fragmentFactory.see(SpecificationLink(s.header, alias = s.header.showWords))
  def see(s: =>SpecificationStructure): Fragment = fragmentFactory.see(SpecificationLink(s.is.header, alias = s.is.header.showWords))
}

