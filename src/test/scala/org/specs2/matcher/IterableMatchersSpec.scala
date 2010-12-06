package org.specs2
package matcher

class IterableMatchersSpec extends SpecificationWithJUnit { def is = 
  
  "contain checks if one or several elements are present in an iterable"                  ^
    { List(1, 2) must contain(1) }                                                        ^
    { List(1, 2, 3) must contain(3, 2) }                                                  ^
    { (List(1, 2) must contain(0)) returns "'List(1, 2)' doesn't contain '0'" }           ^
                                                                                          endp^
  "contain(...).inOrder checks if an iterable contains other elements in the same order"  ^
    { List(1, 2, 3, 4) must contain(2, 4).inOrder }                                       ^
    "and fails when one element is missing"                                               ! order().fail1^ 
    "or if the order is wrong"                                                            ! order().fail2^
                                                                                          end
  case class order() {
    def fail1 = (List(1, 2, 3, 4) must contain(2, 5).inOrder) returns 
                "'List(1, 2, 3, 4)' doesn't contain in order '2, 5'"
    def fail2 = (List(1, 2, 3, 4) must contain(4, 2).inOrder) returns  
                 "'List(1, 2, 3, 4)' doesn't contain in order '4, 2'"
  }

}                                                                                          