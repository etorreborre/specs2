package org.specs2
package form

import control.Property
import execute._
import sys._
import specification.Grouped

class PropSpec extends Specification with Grouped {  def is = s2"""
                                                                    
A Prop is a Field defining an expected and an actual value.

It embeddeds an optional constraint which allows to execute the Prop and see (by default) if
the actual value is equal to the expected value.
                                                                                                                        
 A Prop can be created
   with a single label                                                                    ${g1.e1}
   with a single value (the actual one)                                                   ${g1.e2}
   with a label and actual value                                                          ${g1.e3}
   with a label, the actual value and a binary function returning a result                ${g1.e4}
   with a label, the actual value and a matcher                                           ${g1.e5}
   with the actual value and a muted matcher                                              ${g1.e6}
   with the actual value, the expected value and a muted matcher                          ${g1.e7}

 A Prop can be displayed
   with only the expected value                                                           ${g2.e1}
   with only the actual value                                                             ${g2.e2}
   with only one value when expected == actual                                            ${g2.e3}

 A Prop can be updated
   with the expected value                                                                ${g3.e1}

 A Prop can be executed
   it returns pending if it has no values at all                                          ${g4.e1}
   it returns pending if it only has an actual value                                      ${g4.e2}
   it returns pending if it only has an expected value                                    ${g4.e3}
   it returns success if expected == actual                                               ${g4.e4}
   it returns a failure if expected != actual                                             ${g4.e5}
   it returns an error if there's an exception                                            ${g4.e6}
   it works with a general constraint
     and returns a success if constraint(actual, expected) == success                     ${g4.e7}
     and returns a failure if constraint(actual, expected) fails                          ${g4.e8}
   it works with a matcher constraint
     and returns a success if (actual matcher expected) == success                        ${g4.e9}
     and returns a failure if (actual matcher expected) fails                             ${g4.e10}
                                                                                          """

  val nameProp = Prop("name", "eric")
  val noValues = new Prop("name")
  val actualOnly = Prop(18)
  val expectedOnly = new Prop("", Property(), Property(18))
  val constrained: Prop[String, String] = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher = Prop("name", "eric", contain(_:String))
  
  "creation" - new g1 {
    e1 := noValues.label                       === "name"
    e2 := actualOnly.actual.get                === 18
    e3 := nameProp.actual.get                  === "eric"
    e4 := constrained.label                    === "name"
    e5 := withMatcher("e").execute             === Success("'eric' contains 'e'")
    e6 := Prop("", 1, be_>(0).mute).execute    === Success("")
    e7 := Prop("", 1, 2, be_>(0).mute).execute === Success("")
  }
                                              
  "display" - new g2 {
    e1 := new Prop("name", expected = Property("eric")).toString        === "name: eric (actual: _)"
    e2 := new Prop("name", actual = Property("eric")).toString          === "name: _ (actual: eric)"
    e3 := new Prop("name", Property("eric"), Property("eric")).toString === "name: eric"
  }

  "update" - new g3 {
    e1 := Prop("name", "eric")("paolo").expected.get must_== "paolo"
  }

  "exec" - new g4 with specification.Forms {
    e1  := noValues.execute                             === Pending("No expected value")
    e2  := actualOnly.execute                           === Pending("No expected value")
    e3  := expectedOnly.execute                         === Pending("No actual value")
    e4  := nameProp("eric").execute                     === Success("'eric' is equal to 'eric'")
    e5  := nameProp("eric2").execute.message            === "'eric' is not equal to 'eric2'"
    e6  := nameProp.apply(error("bad")).execute.message === "bad"
    e7  := constrained("e").execute.isSuccess           === true
    e8  := constrained("a").execute.message             === "'eric' doesn't contain 'a'"
    e9  := withMatcher("e").execute.isSuccess           === true
    e10 := withMatcher("a").execute.message             === "'eric' doesn't contain 'a'"
  }
}    