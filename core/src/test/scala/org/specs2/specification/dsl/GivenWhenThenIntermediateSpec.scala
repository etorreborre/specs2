package org.specs2
package specification
package dsl

import script.StepParser

class GivenWhenThenIntermediateSpec extends Specification with GivenWhenThen { def is = s2"""
 a class with an int {1} ${step(anInt)(s1)}
"""

  def anInt: StepParser[Int] = StepParser((_:String).toInt)

  def s1: Int => Unit = (i: Int) => ("got "+i).pp
}


