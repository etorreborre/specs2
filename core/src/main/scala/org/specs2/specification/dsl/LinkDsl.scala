package org.specs2
package specification
package dsl

import core._
import create.FragmentsFactory
import control.ImplicitParameters.ImplicitParam

/**
 * DSL for adding links to other specifications
 */
trait LinkDsl extends FragmentsFactory {

  implicit class linkFragment(text: String) {
    def ~(s: SpecStructure): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text))

    def ~(alias: String, s: SpecStructure): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text, alias = alias))

    def ~(s: SpecStructure, after: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text, after = after))

    def ~(alias: String, s: SpecStructure, after: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text, alias = alias, after = after))

    def ~(alias: String, s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text, alias = alias, after = after, tooltip = tooltip))

    def ~(s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.link(SpecificationLink(s.header, before = text, tooltip = tooltip, after = after))

    def ~(s: SpecificationStructure): Fragment = text ~ s.is
    def ~(alias: String, s: SpecificationStructure): Fragment = text ~ (alias, s.is)
    def ~(s: SpecificationStructure, after: String): Fragment = text ~ (s.is, after)
    def ~(alias: String, s: SpecificationStructure, after: String): Fragment = text ~ (alias, s.is, after)
    def ~(alias: String, s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~ (alias, s.is, after, tooltip)
    def ~(s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~ (s.is, after, tooltip)
  }

  implicit class seeFragment(text: String) {
    def ~/(s: SpecStructure): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text))

    def ~/(alias: String, s: SpecStructure): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text, alias = alias))

    def ~/(s: SpecStructure, after: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text, after = after))

    def ~/(alias: String, s: SpecStructure, after: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text, alias = alias, after = after))

    def ~/(alias: String, s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text, alias = alias, after = after, tooltip = tooltip))

    def ~/(s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.see(SpecificationLink(s.header, before = text, tooltip = tooltip, after = after))

    def ~/(s: SpecificationStructure): Fragment = text ~/ s.is
    def ~/(alias: String, s: SpecificationStructure): Fragment = text ~/ (alias, s.is)
    def ~/(s: SpecificationStructure, after: String): Fragment = text ~/ (s.is, after)
    def ~/(alias: String, s: SpecificationStructure, after: String): Fragment = text ~/ (alias, s.is, after)
    def ~/(alias: String, s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~/ (alias, s.is, after, tooltip)
    def ~/(s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~/ (s.is, after, tooltip)
  }

  def link(s: SpecStructure): Fragment          = fragmentFactory.link(SpecificationLink(s.header, alias = s.header.show))
  def link(s: SpecificationStructure): Fragment = link(s.is)

  def see(s: SpecStructure): Fragment             = fragmentFactory.see(SpecificationLink(s.header, alias = s.header.show))
  def see(s: =>SpecificationStructure)(implicit p: ImplicitParam): Fragment = fragmentFactory.see(SpecificationLink(s.is.header, alias = s.is.header.show))
}
