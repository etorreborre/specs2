package org.specs2
package specification

/**
 * Set of fragments which can be used for formatting
 */
trait FormattingFragments {
  def p          = Fragments.createList(StandardFragments.Br(), StandardFragments.Backtab())
  def br         = StandardFragments.Br()
  def end        = StandardFragments.End()
  def t          = StandardFragments.Tab()
  def t(n: Int)  = StandardFragments.Tab(n)
  def bt         = StandardFragments.Backtab()
  def bt(n: Int) = StandardFragments.Backtab(n)
  def endp       = Fragments.createList(end, StandardFragments.Br())
  def endbr      = Fragments.createList(end, StandardFragments.Br())
}
private[specs2]
object FormattingFragments extends FormattingFragments

