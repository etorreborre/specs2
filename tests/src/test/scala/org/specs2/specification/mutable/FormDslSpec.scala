package org.specs2.specification
package mutable

import org.specs2.form.Form
import org.specs2.matcher.TypedEqual
import org.specs2.specification.create.DefaultFormFragmentFactory
import org.specs2.specification.process.DefaultExecutor
import org.specs2.specification.core.{Env, FormDescription, OwnEnv}
import org.specs2.specification.dsl.mutable.{FormDsl, MutableFragmentBuilder}

class FormDslSpec(val env: Env) extends org.specs2.Spec with TypedEqual with OwnEnv { def is = s2"""
  simple test for the form dsl $e1

"""

  def e1 = {
    DefaultExecutor.executeAll(dsl.insert(Form("test")))(ownEnv).head.description match {
      case f @ FormDescription(_) => f.show === "| test |"
      case other                  => ko("not a form "+other)
    }
  }

  val dsl = new FormDsl with MutableFragmentBuilder with DefaultFormFragmentFactory {}
}
