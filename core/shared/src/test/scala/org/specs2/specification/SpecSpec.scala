package org.specs2
package specification

class SpecSpec extends Specification { def is = s2"""

 Many matchers can be used in a simple spec $e1

"""

  def e1 =
    Seq(1, 2, 3) `must` contain(2)
    Some(1) `must` beSome(1)
}
