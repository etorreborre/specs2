package org.specs2
package specification
package dsl

import data.NamedTag
import core.*
import create.FragmentsFactory

/** Dsl for creating tags
  */
trait TagDsl extends FragmentsFactory { outer =>

  def tag(names: String*): Fragment = fragmentFactory.taggedAs(names*)
  def section(names: String*): Fragment = fragmentFactory.asSection(names*)

  def tag(tag: NamedTag): Fragment = fragmentFactory.markAs(tag)
  def section(tag: NamedTag): Fragment = fragmentFactory.markSectionAs(tag)

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] def xtag = fragmentFactory.taggedAs("x")

  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] def xsection = fragmentFactory.asSection("x")

}
