package org.specs2
package specification
package dsl

import data.NamedTag
import core._
import create.FragmentsFactory

trait TagsDsl extends FragmentsFactory { outer =>

  def tag(names: String*)    : Fragment   = fragmentFactory.tag(names:_*)
  def taggedAs(names: String*) : Fragment = fragmentFactory.taggedAs(names:_*)
  def section(names: String*)  : Fragment = fragmentFactory.section(names:_*)
  def asSection(names: String*): Fragment = fragmentFactory.asSection(names:_*)

  def tag(tag: NamedTag)      : Fragment = fragmentFactory.mark(tag)
  def taggedAs(tag: NamedTag) : Fragment = fragmentFactory.markAs(tag)
  def section(tag: NamedTag)  : Fragment = fragmentFactory.markSection(tag)
  def asSection(tag: NamedTag): Fragment = fragmentFactory.markSectionAs(tag)

  /** shortcut to add tag more quickly when rerunning failed tests */
  private[specs2] def xtag = fragmentFactory.tag("x")
  /** shortcut to add section more quickly when rerunning failed tests */
  private[specs2] def xsection = fragmentFactory.section("x")

}

