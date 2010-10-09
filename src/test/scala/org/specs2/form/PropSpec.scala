package org.specs2
package form
import execute._

class PropSpec extends SpecificationWithJUnit {
  val content = 
"""  
  A Prop is a Field defining an expected and an actual value. 
  It embeddeds an optional constraint which allows to execute the Prop and see (by default) if 
  the actual value is equal to the expected value. 
"""                                                                                       ^
" A Prop can be created"                                                                  ^
"   with a single label"                                                                  ! creation.e1^
"   with a single value (the actual one)"                                                 ! creation.e2^
"   with a label and actual value"                                                        ! creation.e3^
"   with a label, the actual value and a binary function returning a result"              ! creation.e4^
"   with a label, the actual value and a matcher"                                         ! creation.e5^
                                                                                          p^
" A Prop can be displayed"                                                                ^ 
"   with only the expected value"                                                         ! display.e1^
"   with only the actual value"                                                           ! display.e2^ 
"   with both values"                                                                     ! display.e3^
                                                                                          p^
" A Prop can be updated"                                                                  ^
"   with the expected value"                                                              ! update.e1^
                                                                                          p^
" A Prop can be executed"                                                                  ^
"   it returns pending if it has no values at all"                                        ! exec.e1^
"   it returns pending if it only has an actual value"                                    ! exec.e2^
"   it returns pending if it only has an expected value"                                  ! exec.e3^
"   it returns success if expected == actual"                                             ! exec.e4^
"   it returns a failure if expected != actual"                                           ! exec.e5^
"   it works with a general constraint"                                                   ^
"     and returns a success if constraint(actual, expected) == success"                   ! exec.e6^
"     and returns a failure if constraint(actual, expected) fails"                        ! exec.e7^
"   it works with a matcher constraint"                                                   ^
"     and returns a success if (actual matcher expected) == success"                      ! exec.e8^
"     and returns a failure if (actual matcher expected) fails"                           ! exec.e9^
                                                                                          end

  val name = Prop("name", "eric")
  val noValues = Prop("name")
  val actualOnly = Prop(18)
  val constrained = Prop("name", "eric", (s1: String, s2: String) => s1 must contain(s2))
  val withMatcher = Prop("name", "eric", contain(_:String))
  
  object creation {
    def e1 = noValues.label must_== "name"
    def e2 = actualOnly.actual.get must_== 18
    def e3 = name.actual.get must_== "eric"
    def e4 = constrained.label must_== "name"
    def e5 = withMatcher("e").execute must_== Success("'eric' contains 'e'")
  }
                                              
  object display {
    def e1 = new Prop("name", expected = Property("eric")).toString must_== "name: _ (expected: eric)"
    def e2 = new Prop("name", actual = Property("eric")).toString must_== "name: eric (expected: _)"
    def e3 = new Prop("name", Property("eric"), Property("eric")).toString must_== "name: eric (expected: eric)"
  }
  object update {
    def e1 = Prop("name", "eric")("paolo").expected.get must_== "paolo"
  }
  object exec {
    def e1 = noValues.execute must_== pending
    def e2 = actualOnly.execute must_== pending
    def e3 = name.execute must_== pending
    def e4 = name("eric").execute must_== Success("'eric' is equal to 'eric'")
    def e5 = name("eric2").execute.message must_== "'eric' is not equal to 'eric2'"
    def e6 = constrained("e").execute.isSuccess must beTrue
    def e7 = constrained("a").execute.message must_== "'eric' doesn't contain 'a'"
    def e8 = withMatcher("e").execute.isSuccess must beTrue
    def e9 = withMatcher("a").execute.message must_== "'eric' doesn't contain 'a'"
  }
}    