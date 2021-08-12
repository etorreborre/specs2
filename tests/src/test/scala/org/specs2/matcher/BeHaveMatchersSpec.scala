package org.specs2
package matcher

import language.postfixOps

class BeHaveMatchersSpec extends Specification:
  def is = s2"""

  The following ways of using matchers are allowed

    using not to negate matchers
    ${List(1) must not(contain(2))}
    ${!(List(1) must not(contain(1))).isSuccess}

    using not to negate a result
    ${(List(0, 1) must contain(2)).not.isSuccess}

    using matchers with `not`
    ${List(1) must not(beEmpty)}
    ${!((Nil: List[Int]) must not(beEmpty)).isSuccess}
    ${!((Array.empty[Int]) must not(beEmpty)).isSuccess}

    using matchers with and
    ${(1 must (beEqualTo(1) and beEqualTo(1))).isSuccess}
    ${!(1 must (beEqualTo(1) and beEqualTo(2))).isSuccess}

    using matchers with and and `not`
    ${(1 must (beEqualTo(1) and not(beEqualTo(2)))).isSuccess}
    ${(1 must (not(beEqualTo(2)) and beEqualTo(1))).isSuccess}
    ${!(1 must (not(beEqualTo(1)) and not(beEqualTo(2)))).isSuccess}

"""
