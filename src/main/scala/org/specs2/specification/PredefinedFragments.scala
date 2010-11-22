package org.specs2
package specification

import control.LazyParameters._

private[specs2]
trait PredefinedFragments {
  def p = StandardFragments.Par()
  def br = StandardFragments.Br()
  def end = StandardFragments.End()
  def t = StandardFragments.Tab()
  def t(n: Int) = StandardFragments.Tab(n)
  def bt = StandardFragments.Backtab()
  def bt(n: Int) = StandardFragments.Backtab(n)
  def endp = Fragments(end, p)
  def endbr = Fragments(end, br)
}
private[specs2]
object PredefinedFragments extends PredefinedFragments

