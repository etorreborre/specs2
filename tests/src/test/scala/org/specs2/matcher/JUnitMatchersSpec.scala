package org.specs2
package matcher

class JUnitMatchersSpec extends Spec { def is = s2"""

The specs2 matchers can be reused in JUnit test cases

 Results must be thrown as AssertionFailedErrors if failing
   if there is a Success no exception is thrown           ${c.e1}
   if there is a Failure then an AssertionError is thrown ${c.e2}
   
"""

  trait expectations extends JUnitMustMatchers:
    def successExpectation = 1 must ===(1)
    def failedExpectation = 1 must ===(2)

  object c extends expectations:
    def e1 =  successExpectation
    def e2 =  failedExpectation must throwAn[AssertionError]

}
