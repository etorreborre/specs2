package org.specs2
package specification
package create

import core.Fragments

trait FormattingFragments extends FragmentsFactory {
  private val ff = fragmentFactory
  import ff._

  def p          = Fragments(br, br, bt)
  def br         = Break
  def end        = End
  def t          = Tab
  def t(n: Int)  = Tab(n)
  def bt         = Backtab
  def bt(n: Int) = Backtab(n)
}

object FormattingFragments extends FormattingFragments
