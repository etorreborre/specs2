package org.specs2
package specification
package dsl
package mutable

import create.FragmentsFactory

trait TextDsl extends FragmentBuilder with FragmentsFactory {
  implicit class textFragment(s: String) {
    def txt = addFragment(fragmentFactory.text(s))
    def br  = addFragmentBlock {
      addFragment(fragmentFactory.text(s))
      addFragment(fragmentFactory.break)
    }
    def p = addFragmentBlock {
      addFragment(fragmentFactory.break)
      addFragment(fragmentFactory.text(s))
      addFragment(fragmentFactory.break)
      addFragment(fragmentFactory.backtab)
    }
  }
}

