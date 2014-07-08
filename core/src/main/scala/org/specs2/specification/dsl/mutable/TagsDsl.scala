package org.specs2
package specification
package dsl
package mutable

import data.{Tag, NamedTag}
import core.{Fragments, Fragment}

trait TagsDsl extends org.specs2.specification.dsl.TagsDsl with MutableFragmentBuilder { outer =>
  override def tag(names: String*)    : Fragment   = addFragment(fragmentFactory.tag(names:_*))
  override def taggedAs(names: String*) : Fragment = addFragment(fragmentFactory.taggedAs(names:_*))
  override def section(names: String*)  : Fragment = addFragment(fragmentFactory.section(names:_*))
  override def asSection(names: String*): Fragment = addFragment(fragmentFactory.asSection(names:_*))

  override def tag(tag: NamedTag)      : Fragment = addFragment(super.tag(tag))
  override def taggedAs(tag: NamedTag) : Fragment = addFragment(super.taggedAs(tag))
  override def section(tag: NamedTag)  : Fragment = addFragment(super.section(tag))
  override def asSection(tag: NamedTag): Fragment = addFragment(super.asSection(tag))

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] override def xtag = addFragment(fragmentFactory.tag("x"))
  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] override def xsection = addFragment(fragmentFactory.section("x"))

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
