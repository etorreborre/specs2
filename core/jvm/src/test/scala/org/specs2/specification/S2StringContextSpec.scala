package org.specs2
package specification

import org.specs2.matcher.{MustMatchers, StandardMatchResults, ThrownExpectations}
import org.specs2.execute.{Function0Result, StandardResults, Success}
import Function0Result._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.main.Arguments
import org.specs2.specification.create._
import org.specs2.specification.core._
import org.specs2.specification.dsl.FragmentsDsl
import org.specs2.control.ExecuteActions._

class S2StringContextSpec extends Spec { def is = s2"""

 Fragments can be interpolated from a s2 string
  a simple string as Text, aggregating it to the previous Text                    ${exs.e1}
  another fragment                                                                ${exs.e2}
  a result                                                                        ${exs.e3}
  a delayed result                                                                ${exs.e4}
  fragments                                                                       ${exs.e5}
  fragments from a specification                                                  ${exs.e6}
  2 examples                                                                      ${exs.e7}
  a method call                                                                   ${exs.e8}
  consecutive strings must be compacted                                           ${exs.e9}

  when more than one lines are indented they are taken as the description
    when the last line is indented it is taken as the description                 ${desc.e1}
    when more than one lines are indented they are taken as the description       ${desc.e2}
    when more than one lines have a | margin they are taken as the description    ${desc.e3}
    for an auto-example (no text on the last line)                                ${desc.e4}

${step(ee.shutdown)}
"""

  implicit val ee: ExecutionEnv =
    Env().executionEnv

  object exs extends MustMatchers with StandardResults with S2StringContext with ThrownExpectations {

    import DefaultFragmentFactory._

    def e1 = s2"""this is ${"some text"}""".fragmentsList(ee) must haveSize(1)

    def e2 = s2"""this is ${text("some text")}""".fragmentsList(ee) must haveSize(2)

    def e3 = {
      val fragments =
        s2"""this is
            $ok""".fragmentsList(ee)
      fragments must haveSize(2)
      fragments(1).description.show must_== "`ok`"
    }

    def e4 = s2"""this is ${new Function0Result(() => Success())}""".fragmentsList(ee) must haveSize(1)

    def e5 = s2"""this is ${Fragments(text("the"), text(" world"))}""".fragmentsList(ee) must haveSize(3)

    def e6 = s2"""this is $spec""".fragmentsList(ee) must haveSize(2)

    def e7 =
      s2"""
  this should
    create example 1 $ok
    create example 2 $ok""".fragmentsList(ee) must haveSize(4)

    def e8 = {
      val fragments = s2""" ${`a method call`}""".fragmentsList(ee)
      fragments must haveSize(1)
      fragments.head.description.show must_== "`a method call`"
    }

    def e9 = s2"""this is ${"some text"} $ok""".fragmentsList(ee) must haveSize(2)

    def `a method call` = ok

    val spec = new SpecificationStructure {
      outer =>
      def is = SpecStructure.create(SpecHeader(outer.getClass), Arguments(), Fragments(text("the"), text(" world")))
    }

  }

  object desc extends MustMatchers with StandardResults with StandardMatchResults with S2StringContext {

    def e1 =
      getDescription(
        s2"""this is
           an example $ok""") must_== Description.text("an example")

    def e2 =
      getDescription(
        s2"""
  this is
    a multi-line
    example $ok""") must_== Description.text("a multi-line\n    example")

    def e3 =
      getDescription(
s2"""
  this is
    |a multi-line
    |  margin example $ok""") must_== Description.text("a multi-line\n      margin example")

  def e4 =
    getDescription(
      s2"""this is an auto-example
           $ok""") must_== Description.code("ok")

  def getDescription(fs: Fragments): Description =
    fs.examples.runOption(ee).getOrElse(Nil)(0).

      description
  }
}
trait dsl1 extends S2StringContext with FragmentsDsl
object dsl1 extends dsl1
