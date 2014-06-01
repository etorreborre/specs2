package org.specs2
package specification
package dsl
package mutable

import create.FragmentsFactory

trait TextDsl extends FragmentBuilder with FragmentsFactory {
  implicit class text(s: String) {
    def txt = addFragment(fragmentFactory.Text(s))
    def br  = addFragmentBlock {
      addFragment(fragmentFactory.Text(s))
      addFragment(fragmentFactory.Break)
    }
    def p = addFragmentBlock {
      addFragment(fragmentFactory.Text(s))
      addFragment(fragmentFactory.Break)
      addFragment(fragmentFactory.Backtab)
    }
  }
}

