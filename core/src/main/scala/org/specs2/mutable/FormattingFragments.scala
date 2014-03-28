package org.specs2
package mutable

import specification.{FragmentsFragment, Fragments, Fragment}
import org.specs2.specification.StandardFragments

trait FormattingFragments extends specification.FormattingFragments { outer: FragmentsBuilder =>
  import StandardFragments._
  override def t: Tab      = { val f = super.t    ; addFragments(f); f }
  override def bt: Backtab = { val f = super.bt   ; addFragments(f); f }
  override def t(n: Int)  = { val f = super.t(n) ; addFragments(f); f }
  override def bt(n: Int) = { val f = super.bt(n); addFragments(f); f }

  override def p          = { val f = super.p    ; addFragments(f); f }
  override def br         = { val f = super.br   ; addFragments(f); f }
  override def end        = { val f = super.end  ; addFragments(f); f }

  override def endp       = { val f = super.endp ; addFragments(f); f }
  override def endbr      = { val f = super.endbr; addFragments(f); f }

  /**
   * This implicit allows to follow a fragment by a Formatting fragment
   */
  implicit def fragmentAndFormattingFragment[T <: Fragment](f: =>T) = new FragmentAndFormattingFragment(() => f)
  class FragmentAndFormattingFragment[T <: Fragment](private val f: () => T) {
    def newp        = { outer.end; f(); outer.endp }
    def newbr       = { outer.end; f(); outer.endbr }
    def p           = { f(); outer.p     }
    def br          = { f(); outer.br    }
    def end         = { f(); outer.end   }
    def t           = { f(); outer.t     }
    def t(n: Int)   = { f(); outer.t(n)  }
    def bt          = { f(); outer.bt    }
    def bt(n: Int)  = { f(); outer.bt(n) }
    def endp        = { f(); outer.endp  }
    def endbr       = { f(); outer.endbr }
    def lt          = { outer.t    ; f(); outer.bt    }
    def lt(n: Int)  = { outer.t(n) ; f(); outer.bt(n) }
    def blt         = { outer.bt   ; f(); outer.t     }
    def blt(n: Int) = { outer.bt(n); f(); outer.t(n)  }
  }
  /**
   * This implicit allows to follow a Fragments object by a Formatting fragment
   */
  implicit def fragmentsAndFormattingFragment(f: =>Fragments) =
    new FragmentsAndFormattingFragment(() => f)

  class FragmentsAndFormattingFragment(private val f: () => Fragments) {
    def newp        = { outer.end; f(); outer.endp }
    def newbr       = { outer.end; f(); outer.endbr }
    def p           = { f(); outer.p             }
    def br          = { f(); outer.br            }
    def end         = { f(); outer.end           }
    def t           = { f(); outer.t             }
    def t(n: Int)   = { f(); outer.t(n)          }
    def bt          = { f(); outer.bt            }
    def bt(n: Int)  = { f(); outer.bt(n)         }
    def endp        = { f(); outer.endp          }
    def endbr       = { f(); outer.endbr         }
    def lt          = { outer.t    ; f(); outer.bt    }
    def lt(n: Int)  = { outer.t(n) ; f(); outer.bt(n) }
    def blt         = { outer.bt   ; f(); outer.t     }
    def blt(n: Int) = { outer.bt(n); f(); outer.t(n)  }
  }

  /**
   * This implicit allows to follow a FragmentsFragment object by a Formatting fragment
   */
  implicit def fragmentsFragmentAndFormattingFragment(f: =>FragmentsFragment) =
    new FragmentsFragmentAndFormattingFragment(() => f)

  class FragmentsFragmentAndFormattingFragment(private val f: () => FragmentsFragment) {
    def newp        = { outer.end; f(); outer.endp }
    def newbr       = { outer.end; f(); outer.endbr }
    def p           = { f(); outer.p       }
    def br          = { f(); outer.br      }
    def end         = { f(); outer.end     }
    def t           = { f(); outer.t       }
    def t(n: Int)   = { f(); outer.t(n)    }
    def bt          = { f(); outer.bt      }
    def bt(n: Int)  = { f(); outer.bt(n)   }
    def endp        = { f(); outer.endp    }
    def endbr       = { f(); outer.endbr   }
    def lt          = { outer.t    ; f(); outer.bt    }
    def lt(n: Int)  = { outer.t(n) ; f(); outer.bt(n) }
    def blt         = { outer.bt   ; f(); outer.t     }
    def blt(n: Int) = { outer.bt(n); f(); outer.t(n)  }
  }

  /**
   * This implicit allows to follow a string object by a Formatting fragment
   */
  implicit def textAndFormattingFragment(s: =>String) = new TextAndFormattingFragment(() => s)
  class TextAndFormattingFragment(private val s: () => String) {
    def newp        = { outer.end; s().txt; outer.endp }
    def newbr       = { outer.end; s().txt; outer.endbr }
    def p           = { s().txt; outer.p     }
    def br          = { s().txt; outer.br    }
    def end         = { s().txt; outer.end   }
    def t           = { s().txt; outer.t     }
    def t(n: Int)   = { s().txt; outer.t(n)  }
    def bt          = { s().txt; outer.bt    }
    def bt(n: Int)  = { s().txt; outer.bt(n) }
    def endp        = { s().txt; outer.endp  }
    def endbr       = { s().txt; outer.endbr }
    def lt          = { outer.t    ; s(); outer.bt    }
    def lt(n: Int)  = { outer.t(n) ; s(); outer.bt(n) }
    def blt         = { outer.bt   ; s(); outer.t     }
    def blt(n: Int) = { outer.bt(n); s(); outer.t(n)  }
  }

}
