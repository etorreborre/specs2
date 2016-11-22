package org.specs2
package matcher

import execute._

class ResultMatchersSpec extends Spec with ResultMatchers with TypedEqual { def is = s2"""

The ResultMatchers trait provides matchers to check Result instances.

  beSuccessful checks if a Result is a Success
  ${ success must  beSuccessful }
  ${ success must be successful }
  ${ Failure("f") must not beSuccessful }
  ${ Failure("f") must not be successful }
  ${ (1 must_== 1).toResult must be successful }

  beFailing checks if a Result is a Failure
  ${ failure must beFailing }
  ${ Failure("msg") must beFailing(message = "m.*") }
  ${ success must not be failing }
  ${ success must not beFailing }
  ${ (1 must_== 2).toResult must be failing }

  beSuccessful checks if a MatchResult is a Success
  ${ (1 === 1) must  beSuccessful }
  ${ (1 === 1) must be successful }
  ${ (1 !== 1) must not beSuccessful }
  ${ (1 !== 1) must not be successful }

  beFailing checks if a MatchResult is a Failure
  ${ (1 !== 1) must beFailing }
  ${ (1 !== 1) must beFailing(message = "1 == '1'") }
  ${ (1 === 1) must not be failing }
  ${ (1 === 1) must not beFailing }
  ${ (1 !== 1) must be failing }
                                                                                                                        """
}