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
  implicit def fragmentAndFormattingFragment[T <: Fragment](f: T) = new FragmentAndFormattingFragment(() => f)
  class FragmentAndFormattingFragment[T <: Fragment](private val f: () => T) {
    def newp       = { outer.endp; f(); outer.p }
    def p          = { f(); outer.p     }
    def br         = { f(); outer.br    }
    def end        = { f(); outer.end   }
    def t          = { f(); outer.t     }
    def t(n: Int)  = { f(); outer.t(n)  }
    def bt         = { f(); outer.bt    }
    def bt(n: Int) = { f(); outer.bt(n) }
    def endp       = { f(); outer.endp  }
    def endbr      = { f(); outer.endbr }
  }
  /**
   * This implicit allows to follow a Fragments object by a Formatting fragment
   */
  implicit def fragmentsAndFormattingFragment(f: Fragments) = new FragmentsAndFormattingFragment(() => f)
  class FragmentsAndFormattingFragment(private val f: () => Fragments) {
    def newp       = { outer.endp; f(); outer.p }
    def p          = { f(); outer.p             }
    def br         = { f(); outer.br            }
    def end        = { f(); outer.end           }
    def t          = { f(); outer.t             }
    def t(n: Int)  = { f(); outer.t(n)          }
    def bt         = { f(); outer.bt            }
    def bt(n: Int) = { f(); outer.bt(n)         }
    def endp       = { f(); outer.endp          }
    def endbr      = { f(); outer.endbr         }
  }

  /**
   * This implicit allows to follow a FragmentsFragment object by a Formatting fragment
   */
  implicit def fragmentsFragmentAndFormattingFragment(f: FragmentsFragment) = new FragmentsFragmentAndFormattingFragment(() => f)
  class FragmentsFragmentAndFormattingFragment(private val f: () => FragmentsFragment) {
    def newp       = { outer.endp; f(); p }
    def p          = { f(); outer.p       }
    def br         = { f(); outer.br      }
    def end        = { f(); outer.end     }
    def t          = { f(); outer.t       }
    def t(n: Int)  = { f(); outer.t(n)    }
    def bt         = { f(); outer.bt      }
    def bt(n: Int) = { f(); outer.bt(n)   }
    def endp       = { f(); outer.endp    }
    def endbr      = { f(); outer.endbr   }
  }

  /**
   * This implicit allows to follow a string object by a Formatting fragment
   */
  implicit def textAndFormattingFragment(s: String) = new TextAndFormattingFragment(() => s)
  class TextAndFormattingFragment(private val s: () => String) {
    def newp       = { outer.endp; s().txt; outer.p }
    def p          = { s().txt; outer.p     }
    def br         = { s().txt; outer.br    }
    def end        = { s().txt; outer.end   }
    def t          = { s().txt; outer.t     }
    def t(n: Int)  = { s().txt; outer.t(n)  }
    def bt         = { s().txt; outer.bt    }
    def bt(n: Int) = { s().txt; outer.bt(n) }
    def endp       = { s().txt; outer.endp  }
    def endbr      = { s().txt; outer.endbr }
  }

}
