package org.specs2
package specification
package dsl
package mutable

import script.StandardDelimitedStepParsers

class GivenWhenThenIntermediateSpec extends org.specs2.mutable.Specification with dsl.mutable.GivenWhenThen with StandardDelimitedStepParsers {

 "Given a first number {2}".step(anInt) { i =>
   number = i
 }

 "When multiply it by {3}".step(anInt) { j =>
   number = number * j
 }

 "Then I get {6}".example(anInt) { i: Int =>
   number must_== i
 }

  var number = 0
}


