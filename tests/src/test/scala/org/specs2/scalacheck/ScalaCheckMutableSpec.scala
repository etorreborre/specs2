package org.specs2.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import org.specs2.ScalaCheck
import org.specs2.matcher.ResultMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.core.Env
import org.specs2.specification.process.DefaultExecutor

class ScalaCheckMutableSpec(env: Env) extends Specification with ScalaCheck with ResultMatchers {

  "this property must fail (see #581)" >> {
    DefaultExecutor.executeFragments(s2"fail here $failingProperties")(env).map(_.executionResult).head must beFailing
  }


  val failingProperties: Properties = new Properties("Hello") {
    property("ints") =
      forAll(Gen.choose(0, 10))((i: Int) => i ==== i + 1)
  }

}
