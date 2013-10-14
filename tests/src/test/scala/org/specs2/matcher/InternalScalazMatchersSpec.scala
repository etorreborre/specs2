package org.specs2
package matcher

import scalaz._

class InternalScalazMatchersSpec extends Specification with InternalScalazMatchers { def is = s2"""

  Validation
  ${ Success[String, Int](1) must succeedWith(1) }
  ${ Success[String, Int](1) must beSuccessful }
  ${ (Success[String, Int](1) must succeedWith(2)) returns("Success(1) is not a Success(2)") }
  ${ (Success[String, Int](1) must failWith("bad")) returns("Success(1) is not a Failure(bad)") }
  ${ Failure[String, Int]("bad") must failWith("bad") }
  ${ (Failure[String, Int]("bad") must failWith("boo")) returns("Failure(bad) is not a Failure(boo)") }
  ${ (Failure[String, Int]("bad") must succeedWith(2)) returns("Failure(bad) is not a Success(2)") }
                                                                                                                """
}