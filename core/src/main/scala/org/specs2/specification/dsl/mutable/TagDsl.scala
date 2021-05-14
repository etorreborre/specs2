package org.specs2
package specification
package dsl
package mutable

import data.*
import core.{Fragments, Fragment}

/**
 * Dsl for creating tags in a mutable specification
 */
trait TagDsl extends org.specs2.specification.dsl.TagDsl with MutableFragmentBuilder { outer =>
  override def tag(names: String*)    : Fragment   = addFragment(fragmentFactory.tag(names*))
  override def section(names: String*)  : Fragment = addFragment(fragmentFactory.section(names*))

  override def tag(tag: NamedTag)      : Fragment = addFragment(super.tag(tag))
  override def section(tag: NamedTag)  : Fragment = addFragment(super.section(tag))

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] override def xtag = addFragment(fragmentFactory.tag("x"))
  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] override def xsection = addFragment(fragmentFactory.section("x"))

  /**
   * This implicit allows to add tags and sections _after_ the examples
   */
  extension (f: =>Fragment)
    infix def tag(tag: String): Fragment =
      outer.tag(tag)
      f

    infix def section(tag: String): Fragment =
      outer.section(tag)
      f

    infix def tag(tag: NamedTag): Fragment =
      outer.tag(tag)
      f

    infix def section(tag: NamedTag): Fragment =
      outer.section(tag)
      f

  /**
   * This implicit allows to add tags and sections _after_ the examples
   */
  extension (fs: =>Fragments)
    infix def tag(tag: NamedTag): Fragments =
      outer.tag(tag)
      fs

    infix def section(tag: NamedTag): Fragments =
      outer.section(tag)
      fs

}
