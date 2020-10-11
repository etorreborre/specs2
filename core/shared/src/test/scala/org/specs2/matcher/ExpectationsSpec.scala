package org.specs2
package matcher

class ExpectationsSpec extends Specification { def is = s2"""

 StoredExpectations can be sandboxed $e1
 ThrownExpectations can be sandboxed $e2

"""

  def e1 =
    val expectations = new StoredExpectations1 {}
    expectations.check(ok)
    expectations.sandboxMatchResult(ko)

    "ko has not been stored" ==> {
      expectations.storedResults must contain(exactly(ok.toResult))
    }

  def e2 =
    val expectations = new ThrownExpectations1 {}
    expectations.sandboxMatchResult(expectations.check(ko)) must ===(ko)

  trait StoredExpectations1 extends StoredExpectations:
    def check[T](m: MatchResult[T]):
      MatchResult[T] = checkMatchResultFailure(m)

  trait ThrownExpectations1 extends ThrownExpectations:
    def check[T](m: MatchResult[T]): MatchResult[T] = checkMatchResultFailure(m)


}
