package org.specs2
package specification
package dsl

import org.specs2.specification.script.{StandardDelimitedStepParsers, StepParser}

import scala.util.parsing.combinator.RegexParsers

class GivenWhenThenIntermediateSpec extends Specification with GivenWhenThen with StandardDelimitedStepParsers { def is = s2"""
 Given a first number {2}     $g1
 When multiply it by {3}      $w1
 Then I get {6}               $t1
"""
  var number = 0
  def g1 = step(anInt) { i =>
    number = i
  }

  def w1 = step(anInt) { j =>
    number = number * j
  }

  def t1 = example(anInt)((i: Int) => number must_== i)
}


