package org.specs2
package matcher

import execute.*

class ResultMatchersSpec extends Spec with ResultMatchers with TypedEqual {
  def is = s2"""

The ResultMatchers trait provides matchers to check Result instances.

  beSuccessful checks if a Result is a Success
  ${success must beSuccessful}
  ${Failure("f") must not(beSuccessful)}

  beFailing checks if a Result is a Failure
  ${failure must beFailing}
  ${Failure("msg") must beFailing(message = "m.*")}
  ${success must not(beFailing)}

  beSuccessful checks if a Result is a Success
  ${(1 === 1) must beSuccessful}
  ${(1 !== 1) must not(beSuccessful)}

  beFailing checks if a Result is a Failure
  ${(1 !== 1) must beFailing}
  ${(1 !== 1) must beFailing(message = "1 == 1")}
  ${(1 === 1) must not(beFailing)}
                                                                                                                        """
}
