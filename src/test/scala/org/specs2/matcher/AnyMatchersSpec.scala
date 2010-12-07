package org.specs2
package matcher

class AnyMatchersSpec extends SpecificationWithJUnit { def is =
  "beTrue"                                                                                ^
    { true must beTrue }                                                                  ^
    { (false must beTrue).message must_==  "the value is false" }                         ^
                                                                                          p^
  "beFalse"                                                                               ^
    { false must beFalse }                                                                ^
    { (true must beFalse).message must_== "the value is true" }                           ^
                                                                                          p^
  "toSeq allows to transform a single matcher to a matcher checking a Seq"                ^
    { List(1, 2, 3) must ((be_==(_:Int)).toSeq)(Seq(1, 2, 3)) }                           ^
                                                                                          p^
  "toSet allows to transform a single matcher to a matcher checking a Set"                ^
    { Set(1, 2, 3) must ((be_==(_:Int)).toSet)(Set(1, 2, 3)) }                            ^ 
                                                                                          end
}