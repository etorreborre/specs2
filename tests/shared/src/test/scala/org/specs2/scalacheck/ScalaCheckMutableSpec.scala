package org.specs2.scalacheck

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}
import org.specs2.fp.syntax.*
import org.specs2.ScalaCheck
import org.specs2.control.Action
import org.specs2.execute.{Result, Success}
import org.specs2.matcher.ActionMatchers
import org.specs2.specification.core.{Fragment, Env, OwnEnv}
import org.specs2.matcher.ResultMatchers
import org.specs2.mutable.Specification
import org.specs2.specification.process.DefaultExecutor
import language.adhocExtensions

class ScalaCheckMutableSpec extends Specification with ScalaCheck with ResultMatchers with OwnEnv with ActionMatchers:

  "this property must fail (see #581)" >> {
    val action: Action[Result] =
      DefaultExecutor
        .executeFragmentsAction(s2"fail here $failingProperties")(env)
        .flatMap(_.traverse(_.executionResult).map(_.headOption.getOrElse(ok)))

    action must beOk((r: Result) => r must beFailing)
  }

  val failingProperties: Properties = new Properties("Hello") {
    property("ints") = forAll(Gen.choose(0, 10))((i: Int) => i === i + 1)
  }
