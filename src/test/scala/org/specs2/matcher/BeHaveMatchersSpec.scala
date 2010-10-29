package org.specs2
package matcher

class BeHaveMatchersSpec extends SpecificationWithJUnit { def is = 
  "The following ways of using matchers are allowed"                                      ^
  "  using not to negate matchers"                                                        ^
//     { List(1) must not contain(2) }                                                      ^ 
//     { !(List(1) must not contain(1)).isSuccess }                                         ^
//                                                                                          p^
//  "  using be before using a matcher"                                                     ^
//     { (Nil:List[Int]) must be empty }                                                    ^
//     { !(List(1) must be empty).isSuccess }                                               ^
//                                                                                          p^
//  "  using not and be in combination"                                                     ^
//     { List(1) must not be empty }                                                        ^
//     { !((Nil:List[Int]) must not be empty).isSuccess }                                   ^
//                                                                                          p^
//  "  using and and be in combination"                                                     ^
//     { (1 must be equalTo(1) and be equalTo(1)).isSuccess }                               ^
//     { !(1 must be equalTo(1) and be equalTo(2)).isSuccess }                              ^
//                                                                                          p^
//  "  using and, not and be in combination"                                                ^
//     { (1 must be equalTo(1) and not be equalTo(2)).isSuccess }                           ^
//     { (1 must not be equalTo(2) and be equalTo(1)).isSuccess }                           ^
//     { !(1 must not be equalTo(1) and not be equalTo(2)).isSuccess }                      ^
//                                                                                          p^
//  "  using or, not and be in combination"                                                 ^
//     { (1 must be equalTo(1) or be equalTo(2)).isSuccess }                                ^
//     { (1 must be equalTo(2) or be equalTo(1)).isSuccess }                                ^
//     { (1 must not be equalTo(2) or not be equalTo(1)).isSuccess }                        ^
     { (1 must not be equalTo(1) or not be equalTo(2)).isSuccess }                        ^
                                                                                          end
}