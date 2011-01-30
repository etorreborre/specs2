package org.specs2
package matcher

import execute._

class ResultMatchersSpec extends SpecificationWithJUnit { def is =
                                                                                          """
  The ResultMatchers trait provides matchers to check Result instances.
                                                                                          """^
                                                                                          p^
  "beSuccessful checks if a Result is a Success"                                          ^
  { success must beSuccessful }                                                           ^
  { success must be successful }                                                          ^
  { Failure("f") must not beSuccessful }                                                  ^
  { Failure("f") must not be successful }                                                 ^
  { (1 must_== 1).toResult must be successful }                                           ^
                                                                                          end
}