package org.specs2
package matcher

import scalaz._

class ScalazMatchersSpec extends Specification with ScalazMatchers { def is = s2"""

  Validation
  ${ Success[Int](1) must succeedWith(1) }
  ${ Success[Int](1) must beSuccessful }
  ${ (Success[Int](1) must succeedWith(2)) returns("Success(1) is not a Success(2)") }
  ${ (Success[Int](1) must failWith("bad")) returns("Success(1) is not a Failure(bad)") }
  ${ Failure[String]("bad") must failWith("bad") }
  ${ (Failure[String]("bad") must failWith("boo")) returns("Failure(bad) is not a Failure(boo)") }
  ${ (Failure[String]("bad") must succeedWith(2)) returns("Failure(bad) is not a Success(2)") }
                                                                                                                """
}