package org.specs2
package specification
package dsl
package mutable

import create.FragmentsFactory
import org.specs2.specification.core.{Fragments, Fragment}

trait TextDsl extends FragmentBuilder with FragmentsFactory {
  implicit class textFragment(s: String) {
    def txt = addFragment(fragmentFactory.text(s))

    def br: Fragment  = s.txt.br
    def br(n: Int): Fragment = s.txt.br(n)

    def p: Fragment = s.txt.p
    def p(n: Int): Fragment = s.txt.p(n)
  }

  implicit class fragmentFormatting(f: =>Fragment) {
    def br: Fragment  = br(1)
    def br(n: Int): Fragment  = addFragmentBlock {
      val result = f
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      result
    }

    def p: Fragment = p(2)
    def p(n: Int): Fragment = addFragmentBlock {
      val before = math.max(1, n - 1)
      (1 to before).map(_ => addFragment(fragmentFactory.break))
      f
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      addFragment(fragmentFactory.backtab)
    }

    def tab: Fragment = tab(1)
    def tab(n: Int): Fragment = addFragmentBlock {
      f
      addFragment(fragmentFactory.tab(n))
    }

    def backtab: Fragment = backtab(1)
    def backtab(n: Int): Fragment = addFragmentBlock {
      f
      addFragment(fragmentFactory.backtab(n))
    }
  }

  implicit class fragmentsFormatting(fs: =>Fragments) {
    def br: Fragments  = br(1)
    def br(n: Int): Fragments  = addFragmentsBlock {
      val result = fs
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      result
    }

    def p: Fragments = p(2)
    def p(n: Int): Fragments = addFragmentsBlock {
      val before = math.max(1, n - 1)
      (1 to before).map(_ => addFragment(fragmentFactory.break))
      val result = fs
      (1 to n).map(_ => addFragment(fragmentFactory.break))
      addFragment(fragmentFactory.backtab)
      result
    }

    def tab: Fragments = tab(1)
    def tab(n: Int): Fragments = addFragmentsBlock {
      val result = fs
      addFragment(fragmentFactory.tab(n))
      result
    }

    def backtab: Fragments = backtab(1)
    def backtab(n: Int): Fragments = addFragmentsBlock {
      val result = fs
      addFragment(fragmentFactory.backtab(n))
      result
    }

  }

}

