package org.specs2
package matcher

class AnyMatchersSpec extends SpecificationWithJUnit {
  def content =
"  beTrue"                                                                                ^
"    when ok" !  { true must beTrue }                                                     ^
"    when ko" !  { (false must beTrue).message must_==  "the value is false" }            ^
                                                                                          p^
"  beFalse"                                                                               ^
"    when ok" !  { false must beFalse }                                                   ^
"    when ko" !  { (true must beFalse).message must_==  "the value is true" }             ^
                                                                                          end
}