package org.specs2
package matcher

import scala.language.postfixOps

class BeHaveMatchersSpec extends Specification { def is = s2"""

 The following ways of using matchers are allowed

   using not to negate matchers
   ${ List(1) `must` not(contain(2)) }
   ${ !(List(1) `must` not(contain(1))).isSuccess }

   using not to negate match results
   ${ (List(0, 1) `must` contain(2)).not.isSuccess }

   using not and be in combination
   ${ List(1) `must` not(beEmpty) }
   ${ !((Nil:List[Int]) `must` not(beEmpty)).isSuccess }
   ${ !((Array.empty[Int]) `must` not(beEmpty)).isSuccess }

   using and and be in combination
   ${ (1 `must` (beEqualTo(1) `and` beEqualTo(1))).isSuccess }
   ${ !(1 `must` (beEqualTo(1) `and` beEqualTo(2))).isSuccess }

   using and, not and be in combination
   ${ (1 `must` (beEqualTo(1) `and` not(beEqualTo(2)))).isSuccess }
   ${ (1 `must` (not(beEqualTo(2)) `and` beEqualTo(1))).isSuccess }
   ${ !(1 `must` (not(beEqualTo(1)) `and` not(beEqualTo(2)))).isSuccess }

   some matchers can be put on 2 lines, but they may have to be ; separated $e1
                                                                                                                        """
  def e1 =
    Some("") `must` not(beSome);
}
