package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters
import ImplicitParameters._
import specification.core.Fragment
import specification.dsl

trait ExampleDsl extends BlockDsl with dsl.ExampleDsl {
  override implicit def forExample(d: String) = new MutableForExample(d)
  class MutableForExample(d: String) extends ForExample(d) {
    def >>(f: =>Fragment): Unit = describe(d) >> f
    def >>(f: =>Unit)(implicit p: ImplicitParam): Unit = describe(d).>>(f)(p)

    def >>[R : AsResult](r: =>R): Fragment = {
      addFragment(fragmentFactory.Example(d, r))
      addFragment(fragmentFactory.Break)
    }
    def in[R : AsResult](r: =>R): Fragment = d >> r
  }
}


