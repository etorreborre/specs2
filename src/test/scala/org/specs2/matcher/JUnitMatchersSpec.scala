package org.specs2
package matcher
import junit.framework._

class JUnitMatchersSpec extends SpecificationWithJUnit { def is =
                                                                                                  """
  The specs2 matchers can be reused in JUnit test cases
                                                                                                  """^
                                                                                                  p^
  "MatchResults must be thrown as exceptions if failing"                                          ^
    "if there is a MatchSuccess no AssertionFailedError is thrown"                                ! c().e1^
    "if there is a MatchFailure the an AssertionFailedError is thrown"                            ! c().e2^
                                                                                                  end

  case class expectations() extends JUnitMustMatchers {
    def successExpectation = 1 must_== 1
    def failedExpectation = 1 must_== 2
  }
  case class c() {
    def e1 =  expectations().successExpectation
    def e2 =  expectations().failedExpectation must throwAn[AssertionFailedError]
  }

}