package org.specs2
package matcher

import execute._
import StringMatchers._

/**
 * Those definitions help specifying the result messages for matchers
 */
trait ReturnsSyntax extends ExpectationsCreation {

  implicit class Returns[T : AsResult](t: =>T) {
    def returns(m: String) =
      (contain(m) ^^ ((_: Result).message))(createExpectable(ResultExecution.execute(AsResult(t))))

    def returnsMatch(m: String) =
      (beMatching(m) ^^ ((_: Result).message))(createExpectable(ResultExecution.execute(AsResult(t))))

    def returnsResult(m: String) = {
      lazy val r = AsResult(t)
      (contain(m) ^^ { (m: Result) => if (r.isSuccess) "success: "+m.message else "failure: "+m.message })(createExpectable(ResultExecution.execute(r)))
    }
  }
}

object ReturnsSyntax extends ReturnsSyntax with Expectations

