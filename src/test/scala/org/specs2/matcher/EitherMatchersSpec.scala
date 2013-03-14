package org.specs2
package matcher

class EitherMatchersSpec extends Specification { def is = s2"""

  The EitherMatchers trait provides matchers to check Either instances
                                                                                                                        
  beRight checks if an element is Right(_)                                                                            
  ${ Right(1) must beRight(1) }
  ${ Left(1) must not be right(1) }
  ${ Right(1) must beRight.like { case i => i must be_>(0) } }
                                                                                                                        
  beLeft checks if an element is Left(_)                                                                              
  ${ Left(1) must beLeft(1) }
  ${ Right(1) must not be left(1) }
  ${ Left(1) must beLeft.like { case i => i must be_>(0) } }
                                                                                                                        """

}