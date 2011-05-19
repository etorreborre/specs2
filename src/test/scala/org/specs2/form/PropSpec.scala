package org.specs2
package form
import control.Property
import execute._
import sys._

class PropSpec extends Specification {  def is =
                                                                                                                        """
A Prop is a Field defining an expected and an actual value.

It embeddeds an optional constraint which allows to execute the Prop and see (by default) if
the actual value is equal to the expected value.
                                                                                                                        """^
  "A Prop can be created"                                                                                               ^
    "with a single label"                                                                                               ! creation.e1^
    "with a single value (the actual one)"                                                                              ! creation.e2^
    "with a label and actual value"                                                                                     ! creation.e3^
    "with a label, the actual value and a binary function returning a result"                                           ! creation.e4^
    "with a label, the actual value and a matcher"                                                                      ! creation.e5^
    "with the actual value and a muted matcher"                                                                         ! creation.e6^
    "with the actual value, the expected value and a muted matcher"                                                     ! creation.e7^
                                                                                                                        p^
  "A Prop can be displayed"                                                                                             ^
    "with only the expected value"                                                                                      ! display.e1^
    "with only the actual value"                                                                                        ! display.e2^
    "with only one value when expected == actual"                                                                       ! display.e3^
                                                                                                                        p^
  "A Prop can be updated"                                                                                               ^
    "with the expected value"                                                                                           ! update.e1^
                                                                                                                        p^
  "A Prop can be executed"                                                                                               ^
    "it returns pending if it has no values at all"                                                                     ! exec.e1^
    "it returns pending if it only has an actual value"                                                                 ! exec.e2^
    "it returns pending if it only has an expected value"                                                               ! exec.e3^
    "it returns success if expected == actual"                                                                          ! exec.e4^
    "it returns a failure if expected != actual"                                                                        ! exec.e5^
    "it returns an error if there's an exception"                                                                       ! exec.e6^
    "it works with a general constraint"                                                                                ^
      "and returns a success if constraint(actual, expected) == success"                                                ! exec.e7^
      "and returns a failure if constraint(actual, expected) fails"                                                     ! exec.e8^
    "it works with a matcher constraint"                                                                                ^p^
      "and returns a success if (actual matcher expected) == success"                                                   ! exec.e9^
      "and returns a failure if (actual matcher expected) fails"                                                        ! exec.e10^
                                                                                                                        end

  val nameProp = Prop("name", "eric")
  val noValues = new Prop("name")
  val actualOnly = Prop(18)
  val expectedOnly = new Prop("", Property(), Property(18))
  val constrained: Prop[String, String] = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher = Prop("name", "eric", contain(_:String))
  
  object creation {
    def e1 = noValues.label must_== "name"
    def e2 = actualOnly.actual.get must_== 18
    def e3 = nameProp.actual.get must_== "eric"
    def e4 = constrained.label must_== "name"
    def e5 = withMatcher("e").execute must_== Success("'eric' contains 'e'")
    def e6 = Prop("", 1, be_>(0).mute).execute must_== Success("")
    def e7 = Prop("", 1, 2, be_>(0).mute).execute must_== Success("")
  }
                                              
  object display {
    def e1 = new Prop("name", expected = Property("eric")).toString must_== "name: eric (actual: _)"
    def e2 = new Prop("name", actual = Property("eric")).toString must_== "name: _ (actual: eric)"
    def e3 = new Prop("name", Property("eric"), Property("eric")).toString must_== "name: eric"
  }
  object update {
    def e1 = Prop("name", "eric")("paolo").expected.get must_== "paolo"
  }
  object exec extends specification.Forms {
    def e1 = noValues.execute must_== Pending("No expected value")
    def e2 = actualOnly.execute must_== Pending("No expected value")
    def e3 = expectedOnly.execute must_== Pending("No actual value")
    def e4 = nameProp("eric").execute must_== Success("'eric' is equal to 'eric'")
    def e5 = nameProp("eric2").execute.message must_== "'eric' is not equal to 'eric2'"
    def e6 = nameProp.apply(error("bad")).execute.message must_== "bad"
    def e7 = constrained("e").execute.isSuccess must beTrue
    def e8 = constrained("a").execute.message must_== "'eric' doesn't contain 'a'"
    def e9 = withMatcher("e").execute.isSuccess must beTrue
    def e10 = withMatcher("a").execute.message must_== "'eric' doesn't contain 'a'"
  }
}    