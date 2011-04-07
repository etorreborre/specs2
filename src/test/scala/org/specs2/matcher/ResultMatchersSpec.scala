package org.specs2
package matcher

import execute._

class ResultMatchersSpec extends SpecificationWithJUnit with ResultMatchers { def is =
                                                                                                                        """
The ResultMatchers trait provides matchers to check Result instances.
                                                                                                                        """^p^
  "beSuccessful checks if a Result is a Success"                                                                        ^
  { success must  beSuccessful }                                                                                         ^
  { success must be successful }                                                                                        ^
  { Failure("f") must not beSuccessful }                                                                                ^
  { Failure("f") must not be successful }                                                                               ^
  { (1 must_== 1).toResult must be successful }                                                                         ^
                                                                                                                        p^
  "beFailing checks if a Result is a Failure"                                                                           ^
  { failure must beFailing }                                                                                            ^
  { Failure("msg") must beFailing(message = "m.*") }                                                                    ^
  { success must not be failing }                                                                                       ^
  { success must not beFailing }                                                                                        ^
  { (1 must_== 2).toResult must be failing }                                                                            ^
                                                                                                                        end
}