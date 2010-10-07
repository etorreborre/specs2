package org.specs2
package form

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
"   with a label and expected value"                                                      ! creation.e3^
"   with a label, the expected value and a binary function returning a result"            ! creation.e4^
"   with a label, the expected value and a matcher"                                       ! creation.e5^
                                                                                          p^
" A Prop can be displayed"                                                                ^ 
"   with only the expected value"                                                         ! display.e1^
"   with only the actual value"                                                           ! display.e2^ 
"   with only both values"                                                                ! display.e3^
                                                                                          p^
" A Prop can be updated"                                                                  ^
"   with its expected value"                                                              ! update.e1^
                                                                                          end

  val name = Prop("name", "eric")
  
  object creation {
    def e1 = Prop("name").label must_== "name"
    def e2 = Prop(18).actual.get must_== 18
    def e3 = name.expected.get must_== "eric"
    def e4 = Prop("name", "e", (s1: String, s2: String) => s1 must contain(s2)).label must_== "name"
    def e5 = Prop("name", "e", contain(_:String))("eric").execute must_== success
  }
                                              
  object display {
    def e1 = new Prop("name", expected = Property("eric")).toString must_== "name: _ (expected: eric)"
    def e2 = new Prop("name", actual = Property("eric")).toString must_== "name: eric (expected: _)"
    def e3 = new Prop("name", Property("eric"), Property("eric")).toString must_== "name: eric (expected: eric)"
  }
  object update {
    def e1 = Prop("name", "eric")("paolo").expected.get must_== "paolo"
  }
}    