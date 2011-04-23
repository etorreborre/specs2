package org.specs2
package matcher
import java.lang._

class JUnitMatchersSpec extends Specification { def is =
                                                                                                                        """
The specs2 matchers can be reused in JUnit test cases
                                                                                                                        """^
                                                                                                                        p^
  "MatchResults must be thrown as AssertionFailedErrors if failing"                                                     ^
    "if there is a MatchSuccess no exception is thrown"                                                                 ! c.e1^
    "if there is a MatchFailure then an AssertionError is thrown"                                                       ! c.e2^
                                                                                                                        end

  trait expectations extends JUnitMustMatchers {
    def successExpectation = 1 must_== 1
    def failedExpectation = 1 must_== 2
  }
  object c extends expectations {
    def e1 =  successExpectation
    def e2 =  failedExpectation must throwAn[AssertionError]
  }

}