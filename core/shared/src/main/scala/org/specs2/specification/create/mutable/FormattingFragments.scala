package org.specs2.specification.create.mutable

import org.specs2.specification.dsl.mutable.FragmentBuilder

trait FormattingFragments extends org.specs2.specification.create.FormattingFragments with FragmentBuilder {
  override def p          = addFragments(super.p)
  override def br         = addFragment(super.br)
  override def t          = addFragment(super.t)
  override def t(n: Int)  = addFragment(super.t(n))
  override def bt         = addFragment(super.bt)
  override def bt(n: Int) = addFragment(super.bt(n))
  override def end        = addFragment(super.end)
}
