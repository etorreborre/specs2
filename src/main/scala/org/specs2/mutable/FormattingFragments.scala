package org.specs2
package mutable

import specification.{FragmentsFragment, Fragments, Fragment}


trait FormattingFragments extends specification.FormattingFragments { outer: FragmentsBuilder =>
  override def p          = { val f = super.p    ; addFragments(f); f }
  override def br         = { val f = super.br   ; addFragments(f); f }
  override def end        = { val f = super.end  ; addFragments(f); f }
  override def t          = { val f = super.t    ; addFragments(f); f }
  override def t(n: Int)  = { val f = super.t(n) ; addFragments(f); f }
  override def bt         = { val f = super.bt   ; addFragments(f); f }
  override def bt(n: Int) = { val f = super.bt(n); addFragments(f); f }
  override def endp       = { val f = super.endp ; addFragments(f); f }
  override def endbr      = { val f = super.endbr; addFragments(f); f }

  /**
   * This implicit allows to follow a fragment by a Formatting fragment
   */
  implicit def fragmentAndFormattingFragment[T <: Fragment](f: T) = new FragmentAndFormattingFragment(f)
  class FragmentAndFormattingFragment[T <: Fragment](private val f: T) {
    def p          = outer.p
    def br         = outer.br
    def end        = outer.end
    def t          = outer.t
    def t(n: Int)  = outer.t(n)
    def bt         = outer.bt
    def bt(n: Int) = outer.bt(n)
    def endp       = outer.endp
    def endbr      = outer.endbr
  }
  /**
   * This implicit allows to follow a Fragments object by a Formatting fragment
   */
  implicit def fragmentsAndFormattingFragment(f: Fragments) = new FragmentsAndFormattingFragment(f)
  class FragmentsAndFormattingFragment(private val f: Fragments) {
    def p          = outer.p
    def br         = outer.br
    def end        = outer.end
    def t          = outer.t
    def t(n: Int)  = outer.t(n)
    def bt         = outer.bt
    def bt(n: Int) = outer.bt(n)
    def endp       = outer.endp
    def endbr      = outer.endbr
  }

  /**
   * This implicit allows to follow a FragmentsFragment object by a Formatting fragment
   */
  implicit def fragmentsFragmentAndFormattingFragment(f: FragmentsFragment) = new FragmentsFragmentAndFormattingFragment(f)
  class FragmentsFragmentAndFormattingFragment(private val f: FragmentsFragment) {
    def p          = outer.p
    def br         = outer.br
    def end        = outer.end
    def t          = outer.t
    def t(n: Int)  = outer.t(n)
    def bt         = outer.bt
    def bt(n: Int) = outer.bt(n)
    def endp       = outer.endp
    def endbr      = outer.endbr
  }

  /**
   * This implicit allows to follow a string object by a Formatting fragment
   */
  implicit def textAndFormattingFragment(s: String) = new FragmentsFragmentAndFormattingFragment(s)
  class TextAndFormattingFragment(private val s: String) {
    def p          = { s.txt; outer.p     }
    def br         = { s.txt; outer.br    }
    def end        = { s.txt; outer.end   }
    def t          = { s.txt; outer.t     }
    def t(n: Int)  = { s.txt; outer.t(n)  }
    def bt         = { s.txt; outer.bt    }
    def bt(n: Int) = { s.txt; outer.bt(n) }
    def endp       = { s.txt; outer.endp  }
    def endbr      = { s.txt; outer.endbr }
  }

}
