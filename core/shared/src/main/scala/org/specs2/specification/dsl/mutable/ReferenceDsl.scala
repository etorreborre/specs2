package org.specs2
package specification
package dsl
package mutable

import core.{SpecificationStructure, SpecStructure, Fragment}

/**
 * Dsl for creating references in a mutable specification
 */
trait ReferenceDsl extends FragmentBuilder with dsl.ReferenceDsl:

  extension (alias: String):
    override def ~(s: SpecStructure): Fragment =
      addFragment(alias.~(s))

    override def ~(s: => SpecificationStructure, tooltip: String): Fragment =
      addFragment(alias.~(s, tooltip))

  extension (alias: String):
    override def ~/(s: SpecStructure): Fragment =
      addFragment(alias.~/(s))

    override def ~/(s: SpecStructure, tooltip: String): Fragment =
      addFragment(alias.~/(s, tooltip))

    override def ~/(s: =>SpecificationStructure): Fragment =
      addFragment(alias.~/(s))

    override def ~/(s: =>SpecificationStructure, tooltip: String): Fragment =
      addFragment(alias.~/(s, tooltip))
