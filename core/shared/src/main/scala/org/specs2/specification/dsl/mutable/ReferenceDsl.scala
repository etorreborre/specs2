package org.specs2
package specification
package dsl
package mutable

import core.{SpecificationStructure, SpecStructure, Fragment}

/** Dsl for creating references in a mutable specification
  */
trait ReferenceDsl extends FragmentBuilder with dsl.ReferenceDsl:

  extension (alias: String)
    override infix def ~(s: SpecStructure): Fragment =
      addFragment(super.~(alias)(s))

  override infix def ~(s: SpecStructure, tooltip: String): Fragment =
    addFragment(super.~(alias)(s, tooltip))

  override infix def ~(s: =>SpecificationStructure): Fragment =
    addFragment(super.~(alias)(s))

  override infix def ~(s: =>SpecificationStructure, tooltip: String): Fragment =
    addFragment(super.~(alias)(s, tooltip))

  extension (alias: String)
    override infix def ~/(s: SpecStructure): Fragment =
      addFragment(super.~/(alias)(s))

    override infix def ~/(s: SpecStructure, tooltip: String): Fragment =
      addFragment(super.~/(alias)(s, tooltip))

    override infix def ~/(s: =>SpecificationStructure): Fragment =
      addFragment(super.~/(alias)(s))

    override infix def ~/(s: =>SpecificationStructure, tooltip: String): Fragment =
      addFragment(super.~/(alias)(s, tooltip))
