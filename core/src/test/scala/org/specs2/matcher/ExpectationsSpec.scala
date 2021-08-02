package org.specs2
package matcher

import execute.*
import matcher.*
import specification.*

class ExpectationsSpec extends Specification { def is = s2"""

 StoredExpectations can be sandboxed $e1
 ThrownExpectations can be sandboxed $e2

"""

  def e1 =
    val expectations = new StoredExpectations1 {}
    expectations.check(ok)
    expectations.sandboxResult(ko)

    "ko has not been stored" ==> {
      expectations.storedResults must contain(exactly(ok))
    }

  def e2 =
    val expectations = new ThrownExpectations1 {}
    expectations.sandboxResult(expectations.check(ko)) must ===(ko)

  trait StoredExpectations1 extends StoredExpectations:
    def check(r: Result): Result =
      checkResultFailure(r)

  trait ThrownExpectations1 extends ThrownExpectations:
    def check(r: Result): Result =
      checkResultFailure(r)


}
