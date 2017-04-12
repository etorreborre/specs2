package org.specs2
package specification
package dsl
package mutable

import script.StandardDelimitedStepParsers

class GWTIntermediateMutableSpec extends org.specs2.mutable.Spec with dsl.mutable.GWT with StandardDelimitedStepParsers {
 sequential

 addParagraph("adding numbers")

  var number = 0

 "Given a first number {2}".step(anInt) { i =>
   number = i
 }

 "When multiply it by {3}".step(anInt) { j =>
   number = number * j
 }

 "Then I get {6}".example(anInt) { i: Int =>
   number must_== i
 }

}


