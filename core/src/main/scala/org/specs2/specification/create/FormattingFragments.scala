package org.specs2
package specification
package create

import core.Fragments

/**
 * Fragments which can be used to change the display
 * of the Specification: paragraphs, breaks, tabs
 */
trait FormattingFragments extends FragmentsFactory {
  private val factory = fragmentFactory

  def p          = Fragments(br, br, bt)
  def br         = factory.break
  def t          = factory.tab
  def t(n: Int)  = factory.tab(n)
  def bt         = factory.backtab
  def bt(n: Int) = factory.backtab(n)
  def end        = factory.end
}

object FormattingFragments extends FormattingFragments
