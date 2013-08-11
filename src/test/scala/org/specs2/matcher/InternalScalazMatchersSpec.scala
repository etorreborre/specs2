package org.specs2
package matcher

import scalaz._

class InternalScalazMatchersSpec extends Specification with InternalScalazMatchers { def is = s2"""

  Validation
  ${ Success(1) must succeedWith(1) }
  ${ Success(1) must beSuccessful }
  ${ (Success(1) must succeedWith(2)) returns("Success(1) is not a Success(2)") }
  ${ (Success(1) must failWith("bad")) returns("Success(1) is not a Failure(bad)") }
  ${ Failure("bad") must failWith("bad") }
  ${ (Failure("bad") must failWith("boo")) returns("Failure(bad) is not a Failure(boo)") }
  ${ (Failure("bad") must succeedWith(2)) returns("Failure(bad) is not a Success(2)") }
                                                                                                """
}