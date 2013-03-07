package org.specs2
package specification

import runner.{MockClassRunner, ClassRunner}
import io._
import execute.{DecoratedResult, Success, AsResult}
import execute.ResultExecution._
import reporter.{ConsoleReporter, TextResultOutput}

class SpecificationStringContextSpec extends Specification { def is = s2"""

 A user specification can use string interpolation to write the specification fragments ${
   val lines = MockClassRunner().run(new UserInterpolatedSpecification)
   val expected = Seq(
     "user specification",
     "This is an introduction",
     "\\+ One example",
     "x Another example with a failure",
     "ko",
     "! A third example with an error",
     "A normal interpolated value: 100",
     "A normal interpolated string: hello",
     "Total for specification user specification"
   )
   forall(expected) { line => lines.map(_.replace("\n", "")) must containMatch(line) }
 }

"""
}

class UserInterpolatedSpecification extends Specification { def is = s2""" $nocolor ${"user specification".title}
 This is an introduction.

 And some text
      with some code

 One example $e1
 Another example with a failure $e2

 A third example with an error $e3

 A normal interpolated value: $i
 A normal interpolated string: $s
 An interpolated value with an error $errorValue

"""

  val i = 100
  val s = "hello"
  def errorValue = { sys.error("undefined"); "xxx" }

  def e1 = ok
  def e2 = ko
  def e3 = { sys.error("boom"); ok }

}

