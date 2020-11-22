package org.specs2
package specification
package dsl
package mutable

import create.FragmentsFactory
import org.specs2.specification.core.{Fragments, Fragment}

/**
 * Dsl for creating text and formatting fragments in a mutable specification
 */
trait TextDsl extends TextCreation:
  outer =>

  extension (s: String):
    def txt = outer.addText(s)

    def br: Fragment  = s.txt.br
    def br(n: Int): Fragment = s.txt.br(n)

    def p: Fragment = s.txt.p
    def p(n: Int): Fragment = s.txt.p(n)

  extension (f: =>Fragment):
    def br: Fragment =
      br(1)

    def br(n: Int): Fragment  =
      val result = f
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      result

    def p: Fragment = p(2)
    def p(n: Int): Fragment =
      val before = math.max(1, n - 1)
      (1 to before).map(_ => addFragment(fragmentFactory.break))
      f
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      addFragment(fragmentFactory.backtab)

    def tab: Fragment = tab(1)
    def tab(n: Int): Fragment =
      f
      addFragment(fragmentFactory.tab(n))

    def backtab: Fragment = backtab(1)
    def backtab(n: Int): Fragment =
      f
      addFragment(fragmentFactory.backtab(n))

  extension (fs: =>Fragments):
    def br: Fragments  = br(1)
    def br(n: Int): Fragments  =
      val result = fs
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      result

    def p: Fragments = p(2)
    def p(n: Int): Fragments =
      val before = math.max(1, n - 1)
      (1 to before).map(_ => addFragment(fragmentFactory.break))
      val result = fs
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      addFragment(fragmentFactory.backtab)
      result

    def tab: Fragments = tab(1)
    def tab(n: Int): Fragments =
      val result = fs
      addFragment(fragmentFactory.tab(n))
      result

    def backtab: Fragments = backtab(1)
    def backtab(n: Int): Fragments =
      val result = fs
      addFragment(fragmentFactory.backtab(n))
      result

trait TextCreation extends FragmentBuilder with FragmentsFactory:
  outer =>

  def addText(s: String): Fragment =
    addFragment(fragmentFactory.text(s))

  def addParagraph(s: String, n: Int = 2): Fragment =
    val before = math.max(1, n - 1)
    (1 to before).map(_ => addFragment(fragmentFactory.break))
    addFragment(fragmentFactory.text(s))
    (1 to n).map(_ => addFragment(fragmentFactory.break))
    addFragment(fragmentFactory.backtab)

  def addBreak: Fragment = addBreak(1)
  def addBreak(n: Int): Fragment =
    val f = addFragment(fragmentFactory.break)
    (1 until n).toList.map(_ => addFragment(fragmentFactory.break))
    f

  def addTab: Fragment = addTab(1)

  def addTab(n: Int): Fragment =
    addFragment(fragmentFactory.tab(n))

  def addBacktab: Fragment = addBacktab(1)

  def addBacktab(n: Int): Fragment =
    addFragment(fragmentFactory.backtab(n))
