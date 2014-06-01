package org.specs2
package specification
package dsl

import core._
import create.FragmentsFactory

/**
 * DSL for adding links to other specifications
 */
trait LinkDsl extends FragmentsFactory {

  implicit class linkFragment(text: String) {
    def ~(s: SpecStructure): Fragment =
      fragmentFactory.Link(SpecificationLink(s.header, alias = text))

    def ~(s: SpecStructure, after: String): Fragment =
      fragmentFactory.Link(SpecificationLink(s.header, before = text, after = after))

    def ~(alias: String, s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.Link(SpecificationLink(s.header, before = text, alias = alias, after = after, tooltip = tooltip))

    def ~(s: SpecStructure, after: String, tooltip: String): Fragment =
      fragmentFactory.Link(SpecificationLink(s.header, tooltip = tooltip, after = after))

    def ~(s: SpecificationStructure): Fragment = text ~ s.is
    def ~(s: SpecificationStructure, after: String): Fragment = text ~ (s.is, after)
    def ~(alias: String, s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~ (alias, s.is, after, tooltip)
    def ~(s: SpecificationStructure, after: String, tooltip: String): Fragment = text ~ (s.is, after, tooltip)

  }

  def link(s: SpecStructure): Fragment          = fragmentFactory.Link(SpecificationLink(s.header))
  def link(s: SpecificationStructure): Fragment = link(s.is)

}
