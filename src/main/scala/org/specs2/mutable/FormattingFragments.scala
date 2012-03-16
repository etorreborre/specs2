package org.specs2
package mutable

trait FormattingFragments extends specification.FormattingFragments { this: FragmentsBuilder =>
  override def p          = { val f = super.p    ; addFragments(f); f }
  override def br         = { val f = super.br   ; addFragments(f); f }
  override def end        = { val f = super.end  ; addFragments(f); f }
  override def t          = { val f = super.t    ; addFragments(f); f }
  override def t(n: Int)  = { val f = super.t(n) ; addFragments(f); f }
  override def bt         = { val f = super.bt   ; addFragments(f); f }
  override def bt(n: Int) = { val f = super.bt(n); addFragments(f); f }
  override def endp       = { val f = super.endp ; addFragments(f); f }
  override def endbr      = { val f = super.endbr; addFragments(f); f }
}
