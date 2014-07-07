package org.specs2
package specification
package dsl
package mutable

import data.{Tag, NamedTag}
import core.{Fragments, Fragment}

trait TagsDsl extends FragmentsDsl with MutableFragmentBuilder { outer =>
  override def tag(tag: NamedTag)      : Fragment = addFragment(super.tag(tag))
  override def taggedAs(tag: NamedTag) : Fragment = addFragment(super.taggedAs(tag))
  override def section(tag: NamedTag)  : Fragment = addFragment(super.section(tag))
  override def asSection(tag: NamedTag): Fragment = addFragment(super.asSection(tag))

  /**
   * This implicit allows to add tags and sections _after_ the examples
   */
  implicit class FragmentTaggedAs(f: =>Fragment) {
    def tag(tag: NamedTag)     = { outer.tag(tag); f }
    def section(tag: NamedTag) = { outer.tag(tag); f }
  }

  /**
   * This implicit allows to add tags and sections _after_ the examples
   */
  implicit class FragmentsTaggedAs(fs: =>Fragments) {
    def tag(tag: NamedTag)     = { outer.tag(tag); fs }
    def section(tag: NamedTag) = { outer.section(tag); fs }
  }

}
