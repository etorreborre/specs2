package org.specs2
package specification
package dsl

import specification.create.FragmentsFactory
import execute.AsResult
import org.specs2.specification.core.{Text, Execution, Env, Fragment}
import control.ImplicitParameters.ImplicitParam

trait ExampleDsl extends FragmentsFactory { outer =>

  implicit def bangExample(d: String) = new BangExample(d)

  class BangExample(d: String) {
    def !(execution: Execution): Fragment                                       = fragmentFactory.example(Text(d), execution)
    def ![R : AsResult](r: => R): Fragment                                      = fragmentFactory.example(d, r)
    def ![R : AsResult](r: String => R): Fragment                               = fragmentFactory.example(d, r)
    def ![R](r: Env => R)(implicit as: AsResult[R], p: ImplicitParam): Fragment = fragmentFactory.example(d, r)(as, p)
  }
}

trait NoBangExampleDsl extends ExampleDsl {
  override def bangExample(d: String) = super.bangExample(d)
}
