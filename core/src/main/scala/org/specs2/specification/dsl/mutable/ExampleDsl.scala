package org.specs2
package specification
package dsl
package mutable

import execute.AsResult
import control.ImplicitParameters
import ImplicitParameters._
import org.specs2.specification.core.{Env, Fragment}
import specification.dsl

trait ExampleDsl extends BlockDsl with dsl.ExampleDsl {
  implicit def blockExample(d: String) = new BlockExample(d)

  class BlockExample(d: String) {
    def >>(f: =>Fragment): Unit = describe(d) >> f
    def >>(f: =>Unit)(implicit p: ImplicitParam): Unit = describe(d).>>(f)(p)

    def >>[R : AsResult](r: =>R): Fragment = {
      addFragment(fragmentFactory.example(d, r))
      addFragment(fragmentFactory.break)
    }

    def in[R : AsResult](r: =>R): Fragment = d >> r
  }

  override implicit def bangExample(d: String): BangExample =
    new MutableBangExample(d)

  class MutableBangExample(d: String) extends BangExample(d) {
    override def ![R : AsResult](r: => R): Fragment                                      = addFragment(fragmentFactory.example(d, r))
    override def ![R : AsResult](r: String => R): Fragment                               = addFragment(fragmentFactory.example(d, r))
    override def ![R](r: Env => R)(implicit as: AsResult[R], p: ImplicitParam): Fragment = addFragment(fragmentFactory.example(d, r)(as, p))
  }
}

trait NoBlockExample extends ExampleDsl {
  override def blockExample(d: String) = super.blockExample(d)
}


