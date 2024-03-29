package org.specs2
package form

import control.Property
import control.Properties.*
import execute.*

import sys.error
import matcher.*
import Matcher.given
import org.specs2.execute.ResultImplicits.combineResult

class PropSpec extends Spec with TypedEqual with PropSyntax {
  def is = s2"""

A Prop is a Field defining an expected and an actual value.

It embeds an optional constraint which allows to execute the Prop and see (by default) if
the actual value is equal to the expected value.

Creation
========

 A Prop can be created
   with a single label                                                     $creation1
   with a single value (the actual one)                                    $creation2
   with a label and actual value                                           $creation3
   with a label, the actual value and a binary function returning a result $creation4
   with a label, the actual value and a matcher                            $creation5
   with the actual value and a muted matcher                               $creation6
   with the actual value, the expected value and a muted matcher           $creation7
   with a label, the actual value and a matcher for the actual value       $creation8

Display
=======

 A Prop can be displayed
   with only the expected value                $display1
   with only the actual value                  $display2
   with only one value when expected == actual $display3

Update
======

 A Prop can be updated
   with the expected value   $update1

Execution
=========

 A Prop can be executed
   it returns pending if it has no values at all                          $execute1
   it returns pending if it only has an actual value                      $execute2
   it returns pending if it only has an expected value                    $execute3
   it returns success if expected == actual                               $execute4
   it returns a failure if expected != actual                             $execute5
   it returns an error if there's an exception                            $execute6
   it works with a general constraint
     and returns a success if constraint(actual, expected) == success     $execute7
     and returns a failure if constraint(actual, expected) fails          $execute8
   it works with a matcher constraint
     and returns a success if (actual matcher expected) == success        $execute9
     and returns a failure if (actual matcher expected) fails             $execute10

"""

  val nameProp: Prop[String, String] = Prop("name", "eric")
  val noValues: Prop[String, String] = Prop("name")
  val actualOnly: Prop[Int, Int] = Prop(18)
  val expectedOnly: Prop[Int, Int] = Prop("", Property[Int](), Property(18))
  val constrained: Prop[String, String] = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher: Prop[String, String] = Prop("name", "eric", contain(_: String))

  def creation1 = noValues.label === "name"
  def creation2 = actualOnly.actual.toOption === Some(18)
  def creation3 = nameProp.actual.toOption === Some("eric")
  def creation4 = constrained.label === "name"
  def creation5 = withMatcher("f").execute === Failure("eric doesn't contain 'f'")
  def creation6 = Prop("", 1, be_>(0).mute).execute === Success("")
  def creation7 = Prop("", 1, 2, be_>(0).mute).execute === Success("")
  def creation8 = {
    (Prop[Int]("value", 1, be_>[Int](0)).execute === Success("1 is strictly greater than 0")) and
      (Prop[Int]("value", 1, be_<(0)).execute === Failure("1 is greater or equal than 0")) and
      (Prop[Int]("value", 1).must(be_>[Int](0)).execute === Success("1 is strictly greater than 0")) and
      (Prop[Int]("value", 1).must(be_<(0)).execute === Failure("1 is greater or equal than 0"))
  }

  def display1 = Prop("name", expected = Property("fanny")).toString === "name: _ (expected: fanny)"
  def display2 = Prop("name", actual = Property("eric")).toString === "name: eric"
  def display3 = Prop("name", Property("eric"), Property("eric")).toString === "name: eric"

  def update1 = Prop("name", "eric")("paolo").expected.toOption must ===(Some("paolo"))

  def execute1 = noValues.execute === Pending("No expected value")
  def execute2 = actualOnly.execute === Pending("No expected value")
  def execute3 = expectedOnly.execute === Pending("No actual value")
  def execute4 = nameProp("eric").execute === Success("'eric' == 'eric'")
  def execute5 = nameProp("eric2").execute.message === "'eric' != 'eric2'"
  def execute6 = nameProp.apply(error("bad")).execute.message === "java.lang.RuntimeException: bad"
  def execute7 = constrained("e").execute.isSuccess === true
  def execute8 = constrained("a").execute.message === "eric doesn't contain 'a'"
  def execute9 = withMatcher("e").execute.isSuccess === true
  def execute10 = withMatcher("a").execute.message === "eric doesn't contain 'a'"

}
