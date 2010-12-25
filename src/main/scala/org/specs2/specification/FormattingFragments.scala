package org.specs2
package specification

import control.LazyParameters._

/**
 * Set of fragments which can be used for formatting
 */
trait FormattingFragments {
  def p = Fragments.create(br, bt)
  def br = StandardFragments.Br()
  def end = StandardFragments.End()
  def t = StandardFragments.Tab()
  def t(n: Int) = StandardFragments.Tab(n)
  def bt = StandardFragments.Backtab()
  def bt(n: Int) = StandardFragments.Backtab(n)
  def endp = Fragments.create(end, br, br)
  def endbr = Fragments.create(end, br)
}
private[specs2]
object FormattingFragments extends FormattingFragments

