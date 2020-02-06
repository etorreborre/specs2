package org.specs2
package form

import control.Property
import execute._
import sys.error
import matcher._
import MatchersImplicits._

class PropSpec extends Spec with TypedEqual {  def is = s2"""

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

  val nameProp = Prop("name", "eric")
  val noValues = new Prop("name")
  val actualOnly = Prop(18)
  val expectedOnly = new Prop("", Property(), Property(18))
  val constrained: Prop[String, String] = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher = Prop("name", "eric", contain(_:String))

  def creation1 = noValues.label                       === "name"
  def creation2 = actualOnly.actual.toOption           === Some(18)
  def creation3 = nameProp.actual.toOption             === Some("eric")
  def creation4 = constrained.label                    === "name"
  def creation5 = withMatcher("e").execute             === Success("eric contains 'e'")
  def creation6 = Prop("", 1, be_>(0).mute).execute    === Success("")
  def creation7 = Prop("", 1, 2, be_>(0).mute).execute === Success("")

  def display1 = new Prop("name", expected = Property("fanny")).toString       === "name: _ (expected: fanny)"
  def display2 = new Prop("name", actual = Property("eric")).toString          === "name: eric"
  def display3 = new Prop("name", Property("eric"), Property("eric")).toString === "name: eric"

  def update1 = Prop("name", "eric")("paolo").expected.toOption must_== Some("paolo")

  def execute1  = noValues.execute                             === Pending("No expected value")
  def execute2  = actualOnly.execute                           === Pending("No expected value")
  def execute3  = expectedOnly.execute                         === Pending("No actual value")
  def execute4  = nameProp("eric").execute                     === Success("eric == 'eric'")
  def execute5  = nameProp("eric2").execute.message            === "'eric' != 'eric2'"
  def execute6  = nameProp.apply(error("bad")).execute.message === "java.lang.RuntimeException: bad"
  def execute7  = constrained("e").execute.isSuccess           === true
  def execute8  = constrained("a").execute.message             === "eric doesn't contain 'a'"
  def execute9  = withMatcher("e").execute.isSuccess           === true
  def execute10 = withMatcher("a").execute.message             === "eric doesn't contain 'a'"

}
