package org.specs2
package form

import control.Property
import execute._
import sys._
import specification._
import matcher._
import MatchersImplicits._

class PropSpec extends script.Spec with Grouped with TypedEqual {  def is = s2"""
                                                                    
A Prop is a Field defining an expected and an actual value.

It embeddeds an optional constraint which allows to execute the Prop and see (by default) if
the actual value is equal to the expected value.

Creation
========

 A Prop can be created
   + with a single label
   + with a single value (the actual one)
   + with a label and actual value
   + with a label, the actual value and a binary function returning a result
   + with a label, the actual value and a matcher
   + with the actual value and a muted matcher
   + with the actual value, the expected value and a muted matcher

Display
=======

 A Prop can be displayed
   + with only the expected value
   + with only the actual value
   + with only one value when expected == actual

Update
======

 A Prop can be updated
   + with the expected value

Execution
=========

 A Prop can be executed
   + it returns pending if it has no values at all
   + it returns pending if it only has an actual value
   + it returns pending if it only has an expected value
   + it returns success if expected == actual
   + it returns a failure if expected != actual
   + it returns an error if there's an exception
   it works with a general constraint
     + and returns a success if constraint(actual, expected) == success
     + and returns a failure if constraint(actual, expected) fails
   it works with a matcher constraint
     + and returns a success if (actual matcher expected) == success
     + and returns a failure if (actual matcher expected) fails
                                                                                          """

  val nameProp = Prop("name", "eric")
  val noValues = new Prop("name")
  val actualOnly = Prop(18)
  val expectedOnly = new Prop("", Property(), Property(18))
  val constrained: Prop[String, String] = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher = Prop("name", "eric", contain(_:String))
  
  "creation" - new g1 {
    e1 := noValues.label                       === "name"
    e2 := actualOnly.actual.toOption           === Some(18)
    e3 := nameProp.actual.toOption             === Some("eric")
    e4 := constrained.label                    === "name"
    e5 := withMatcher("e").execute             === Success("eric contains 'e'")
    e6 := Prop("", 1, be_>(0).mute).execute    === Success("")
    e7 := Prop("", 1, 2, be_>(0).mute).execute === Success("")
  }
                                              
  "display" - new g2 {
    e1 := new Prop("name", expected = Property("fanny")).toString       === "name: _ (expected: fanny)"
    e2 := new Prop("name", actual = Property("eric")).toString          === "name: eric"
    e3 := new Prop("name", Property("eric"), Property("eric")).toString === "name: eric"
  }

  "update" - new g3 {
    e1 := Prop("name", "eric")("paolo").expected.toOption must_== Some("paolo")
  }

  "exec" - new g4 with form.FormsBuilder {
    e1  := noValues.execute                             === Pending("No expected value")
    e2  := actualOnly.execute                           === Pending("No expected value")
    e3  := expectedOnly.execute                         === Pending("No actual value")
    e4  := nameProp("eric").execute                     === Success("eric == 'eric'")
    e5  := nameProp("eric2").execute.message            === "'eric' != 'eric2'"
    e6  := nameProp.apply(error("bad")).execute.message === "java.lang.RuntimeException: bad"
    e7  := constrained("e").execute.isSuccess           === true
    e8  := constrained("a").execute.message             === "eric doesn't contain 'a'"
    e9  := withMatcher("e").execute.isSuccess           === true
    e10 := withMatcher("a").execute.message             === "eric doesn't contain 'a'"
  }
}    
