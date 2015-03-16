package org.specs2
package execute

import org.scalacheck._, Gen._, Arbitrary._

class ResultImplicitsSpec extends Specification with ScalaCheck with ResultImplicits { def is = s2"""

 2 results are equivalent if they are both true or both false at the same time $equivalent

"""

  def equivalent = prop { r1: Result =>
    (r1 <==> r1) &&
    (r1 <==> r1.not).not
  }.set(minTestsOk = 5)

  implicit def ArbitrarySuccessOrFailure: Arbitrary[Result] = Arbitrary {
    Gen.oneOf(success, failure)
  }

}
