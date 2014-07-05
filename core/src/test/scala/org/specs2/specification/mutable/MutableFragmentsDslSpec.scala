package org.specs2.specification
package mutable

import core._
import create.DefaultFragmentFactory
import DefaultFragmentFactory._
import core.{Env, Results, Fragment}
import dsl.mutable.{MutableFragmentsDsl, MutableFragmentBuilder}

class MutableFragmentsDslSpec extends org.specs2.Specification { def is = s2"""

  create examples
    with a string and a result $e1
    with a string and a for loop $e2

  create blocks
    with simple examples $e3
    with a for loop $e4

  set a title on the specification $e5

  set arguments on the specification $e6

"""

  def e1 = fragments(new dsl { "e1" in ok }) must contain(exactly(break, example("e1", ok), break))

  def e2 = fragments(new dsl { "e1" in Results.foreach(1 to 2)(i => i === i) }) must
    contain(exactly(break, example("e1", ok), break))

  def e3 = fragments(new dsl {
    "this" should {
      "e1" in ok
      "e2" in ok
    }
  }).map(_.description) must
    contain(exactly(Seq(
      break,
      start, break,
      text("this should"), tab, break,
      example("e1", ok), break,
      example("e2", ok),
      break, backtab, end).map(_.description):_*))

  def e4 = fragments(new dsl {
    "this" should {
      (1 to 2).repeat { i => "e"+i in ok }
    }
  }).map(_.description) must
    contain(exactly(Seq(
      break,
      start, break,
      text("this should"), tab, break,
      example("e1", ok), break,
      example("e2", ok),
      break, backtab, end).map(_.description):_*))

  def e5 = structure(new dsl {
    "have a title".title
    "this" should { "have an example" in ok }
  }).header.title must beSome("have a title")

  def e6 = structure(new dsl {
    args(plan = true)
    "this" should { "have an example" in ok }
  }).arguments.plan must beTrue

  def fragments(dsl1: dsl) = structure(dsl1).fragments.fragments

  def structure(dsl1: dsl) = {
    val env = Env()
    dsl1.specificationStructure(env)
  }

  trait dsl extends MutableFragmentBuilder with MutableFragmentsDsl
}
