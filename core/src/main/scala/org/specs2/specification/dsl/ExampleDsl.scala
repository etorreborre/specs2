package org.specs2
package specification
package dsl

import specification.create.FragmentsFactory
import execute.AsResult
import specification.core.{Env, Fragment}
import control.ImplicitParameters.ImplicitParam


trait ExampleDsl extends FragmentsFactory { outer =>

  implicit def forExample(d: String) = new ForExample(d)
  class ForExample(d: String) {
    def ![R : AsResult](r: => R): Fragment                                      = fragmentFactory.Example(d, r)
    def ![R : AsResult](r: String => R): Fragment                               = fragmentFactory.Example(d, r)
    def ![R](r: Env => R)(implicit as: AsResult[R], p: ImplicitParam): Fragment = fragmentFactory.Example(d, r)(as, p)
  }
}

trait NoExampleDsl extends ExampleDsl {
  override def forExample(d: String) = super.forExample(d)
}
