package org.specs2
package matcher

class BeHaveMatchersSpec extends SpecificationWithJUnit { def is = 
  
  "The following ways of using matchers are allowed"                                      ^
  "  using not to negate matchers"                                                        ^
     { List(1) must not contain(2) }                                                      ^
     { !(List(1) must not contain(1)).isSuccess }                                         ^
                                                                                          p^
  "  using be before using a matcher"                                                     ^
     { (Nil:List[Int]) must be empty }                                                    ^
     { !(List(1) must be empty).isSuccess }                                               ^
                                                                                          p^
  "  using not and be in combination"                                                     ^
     { List(1) must not be empty }                                                        ^
     { !((Nil:List[Int]) must not be empty).isSuccess }                                   ^
                                                                                          p^
  "  using and and be in combination"                                                     ^
     { pending }^
  //     { List(1) must be empty and not be empty}                                            ^
                                                                                          end
}