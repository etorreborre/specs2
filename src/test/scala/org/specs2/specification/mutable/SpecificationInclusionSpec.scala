package org.specs2
package specification
package mutable

import _root_.org.specs2.mutable.{Specification => Spec}

class SpecificationInclusionSpec extends Spec {

  "A specification can be included in another one" >> {
    val spec1 = new Spec { "spec1".title; "ex1" >> ok }
    val spec2 = new Spec { "spec2".title; "ex2" >> ok; include(spec1) }
    spec2.content.fragments.map(_.toString) === Seq(
      "SpecStart(spec2)",
      "Example(ex2)",
      "SpecStart(spec1)",
      "Example(ex1)",
      "SpecEnd(spec1)",
      "SpecEnd(spec2)"
    )
  }

  "A specification can be nested in another one" >> {
    val spec1 = new Spec { "spec1".title; "ex1" >> ok }
    val spec2 = new Spec { "spec2".title; "ex2" >> ok; inline(spec1) }
    spec2.content.fragments.map(_.toString) === Seq(
      "SpecStart(spec2)",
      "Example(ex2)",
      "Example(ex1)",
      "SpecEnd(spec2)"
    )
  }
}
