package org.specs2
package specification
package dsl
package mutable

import create.FragmentsFactory
import org.specs2.specification.core.{Fragments, Fragment}

trait TextDsl extends FragmentBuilder with FragmentsFactory {
  implicit class textFragment(s: String) {
    def txt = addFragment(fragmentFactory.text(s))
    def br  = s.txt.br
    def p   = s.txt.p
  }

  implicit class fragmentFormatting(f: =>Fragment) {
    def br  = addFragmentBlock {
      f
      addFragment(fragmentFactory.break)
    }
    def p = addFragmentBlock {
      addFragment(fragmentFactory.break)
      f
      addFragment(fragmentFactory.break)
      addFragment(fragmentFactory.backtab)
    }
  }

  implicit class fragmentsFormatting(fs: =>Fragments) {
    def br  = addFragmentBlock {
      fs
      addFragment(fragmentFactory.break)
    }
    def p = addFragmentBlock {
      addFragment(fragmentFactory.break)
      fs
      addFragment(fragmentFactory.break)
      addFragment(fragmentFactory.backtab)
    }
  }

}

