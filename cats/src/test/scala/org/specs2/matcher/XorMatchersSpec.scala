package org.specs2
package matcher

import cats.data._, Xor._

class XorMatchersSpec extends Spec with ResultMatchers with XorMatchers { def is = s2"""

 The XorMatchers trait provides matchers to check Xor instances

  beXorRight checks if an element is Right(_)                                                                            
  ${ Right(1) must beXorRight(1) }
  ${ Right(1) must beXorRight((i: Int) => i must be_>(0)) }
  ${ Right(1) must beXorRight(Seq(true, true)) }
  ${ Right(1) must beXorRight(===(1)) }
  ${ Right(Seq(1)) must beXorRight(===(Seq(1))) }
  ${ Right(1) must beXorRight.like { case i => i must be_>(0) } }
  ${ (Right(1) must beXorRight.like { case i => i must be_<(0) }) returns "Right(1) is Right but 1 is not less than 0" }

  beXorLeft checks if an element is Left(_)                                                                              
  ${ Left(1) must beXorLeft(1) }
  ${ Left(1) must beXorLeft(===(1)) }
  ${ Left(1) must beXorLeft.like { case i => i must be_>(0) } }

"""
}

