package org.specs2
package matcher

import scalaz._
import Scalaz._

class ScalazMatchersSpec extends Specification with ScalazMatchers {   def is =

  "Typecheck equals"                                                            ^
  "beEqualTo can use the Equal typeclass to check the equality of 2 values"     ^
    { MyValue("a", 1) must equal(MyValue("a",  -1))                             }^
    { MyValue("a") must not equal(MyValue("b"))                                 }^
                                                                                endp^
  "Validations"                                                                 ^
  "`Success` matching mirrors `Right` matching"                                 ^
    { 1.success must be successful (1)                                          }^
    { 1.right   must be right      (1)                                          }^
    { 1.success must not be successful (2)                                      }^
    { 1.right   must not be right      (2)                                      }^
    { 1.success must beSuccessful (1)                                           }^
    { 1.right   must beRight      (1)                                           }^
    { 1.success must not beSuccessful (2)                                       }^
    { 1.right   must not beRight      (2)                                       }^
    { 1.success must beSuccessful                                               }^
    { 1.right   must beRight                                                    }^
    { 1.success must not beFailing                                              }^
    { 1.right   must not beLeft                                                 }^
    { 1.success must be successful                                              }^
    { 1.right   must be right                                                   }^
    { 1.success must not be failing                                             }^
    { 1.right   must not be left                                                }^
    { List(1, 2).success must beSuccessful.like { case 1 :: _ => ok }           }^
    { List(1, 2).right   must beRight.like      { case 1 :: _ => ok }           }^
                                                                                endp^
  "`Failure` matching mirrors `Left` matching"                                  ^
    { 1.fail must be failing (1)                                                }^
    { 1.left must be left    (1)                                                }^
    { 1.fail must not be failing (2)                                            }^
    { 1.left must not be left    (2)                                            }^
    { 1.fail must beFailing (1)                                                 }^
    { 1.left must beLeft    (1)                                                 }^
    { 1.fail must not beFailing (2)                                             }^
    { 1.left must not beLeft    (2)                                             }^
    { 1.fail must beFailing                                                     }^
    { 1.left must beLeft                                                        }^
    { 1.fail must not beSuccessful                                              }^
    { 1.left must not beRight                                                   }^
    { 1.fail must be failing                                                    }^
    { 1.left must be left                                                       }^
    { 1.fail must not be successful                                             }^
    { 1.left must not be right                                                  }^
    { List(1, 2).fail must beFailing.like { case 1 :: _ => ok }                 }^
    { List(1, 2).left must beLeft.like    { case 1 :: _ => ok }                 }^
                                                                                end

  case class MyValue[T](t: T, i: Int = 0)
  implicit def myValueEqual[T]: Equal[MyValue[T]] = new Equal[MyValue[T]] {
    def equal(a1: MyValue[T], a2: MyValue[T]) = (a1.t == a2.t) && (math.abs(a1.i) == math.abs(a2.i))
  }

}

