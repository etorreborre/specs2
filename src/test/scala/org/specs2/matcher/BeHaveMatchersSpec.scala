package org.specs2
package matcher

class BeHaveMatchersSpec extends Specification { def is = s2"""

 The following ways of using matchers are allowed

   using not to negate matchers
   ${ List(1) must not contain(2) }
   ${ !(List(1) must not contain(1)).isSuccess }

   using be before using a matcher
   ${ (Nil:List[Int]) must be empty }
   ${ !(List(1) must be empty).isSuccess }

   using not and be in combination
   ${ List(1) must not be empty }
   ${ !((Nil:List[Int]) must not be empty).isSuccess }

   using and and be in combination
   ${ (1 must be equalTo(1) and be equalTo(1)).isSuccess }
   ${ !(1 must be equalTo(1) and be equalTo(2)).isSuccess }

   using and, not and be in combination
   ${ (1 must be equalTo(1) and not be equalTo(2)).isSuccess }
   ${ (1 must not be equalTo(2) and be equalTo(1)).isSuccess }
   ${ !(1 must not be equalTo(1) and not be equalTo(2)).isSuccess }

   using or, not and be in combination
   ${ (1 must be equalTo(1) or be equalTo(2)).isSuccess }
   ${ (1 must be equalTo(2) or be equalTo(1)).isSuccess }
   ${ (1 must not be equalTo(2) or not be equalTo(1)).isSuccess }
   ${ (1 must not be equalTo(1) or not be equalTo(2)).isSuccess }

   some matchers can be put on 2 lines, but they may have to be ; separated $e1
                                                                                                                        """

  def e1 = {
    Some("") must not beSome;
    Some("") must be some
  }
}