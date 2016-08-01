package org.specs2
package matcher

import scalaz.{ Failure, Success }

class ValidationMatchersSpec extends Spec with ValidationMatchers with ResultMatchers { def is = s2"""

 The ValidationMatchers trait provides matchers to check Validation instances

  beSuccess checks if an element is Success(_)
  ${ Success(1) must beSuccess(1) }
  ${ Success(1) must beSuccess((i: Int) => i must be_>(0)) }
  ${ Success(1) must beSuccess(Seq(true, true)) }
  ${ Success(1) must beSuccess(===(1)) }
  ${ Success(Seq(1)) must beSuccess(===(Seq(1))) }
  ${ Failure(1) must not be right(1) }
  ${ Success(1) must beSuccess.like { case i => i must be_>(0) } }
  ${ (Success(1) must beSuccess.like { case i => i must be_<(0) }) returns "Success(1) is Success but 1 is not less than 0" }

  beFailure checks if an element is Failure(_)
  ${ Failure(1) must beFailure(1) }
  ${ Failure(1) must beFailure(===(1)) }
  ${ Success(1) must not be left(1) }
  ${ Failure(1) must beFailure.like { case i => i must be_>(0) } }
                                                                                                                        """

}
