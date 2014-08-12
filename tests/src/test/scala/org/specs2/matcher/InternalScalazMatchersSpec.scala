package org.specs2
package matcher

import scalaz._, Scalaz._

class InternalScalazMatchersSpec extends Specification with InternalScalazMatchers { def is = s2"""

  Validation
  ${ 1.success must succeedWith(1) }
  ${ 1.success must beSuccessful }
  ${ (1.success must succeedWith(2)) returns("Success(1) is not a Success(2)") }
  ${ (1.success must failWith("bad")) returns("Success(1) is not a Failure(bad)") }
  ${ "bad".failure must failWith("bad") }
  ${ ("bad".failure must failWith("boo")) returns("Failure(bad) is not a Failure(boo)") }
  ${ ("bad".failure must succeedWith(2)) returns("Failure(bad) is not a Success(2)") }
                                                                                                                """
}