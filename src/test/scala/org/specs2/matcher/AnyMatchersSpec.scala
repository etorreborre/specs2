package org.specs2
package matcher

class AnyMatchersSpec extends SpecificationWithJUnit { def is =
  "beTrue matches true values"                                                            ^
  { true must beTrue }                                                                    ^
  { (false must beTrue).message must_==  "the value is false" }                           ^
                                                                                          p^
  "beFalse matches false values"                                                          ^
  { false must beFalse }                                                                  ^
  { (true must beFalse).message must_== "the value is true" }                             ^
                                                                                          p^
  "beLike matches objects against a pattern"                                              ^
  { List(1, 2) must beLike { case List(a, b) => ok } }                                    ^
  { List(1, 2) must beLike { case List(a, b) => (a + b) must_== 3 } }                     ^
                                                                                          p^
  "toSeq allows to transform a single matcher to a matcher checking a Seq"                ^
  { List(1, 2, 3) must ((be_==(_:Int)).toSeq)(Seq(1, 2, 3)) }                             ^
                                                                                          p^
  "toSet allows to transform a single matcher to a matcher checking a Set"                ^
  { Set(1, 2, 3) must ((be_==(_:Int)).toSet)(Set(1, 2, 3)) }                              ^ 
                                                                                          end
}