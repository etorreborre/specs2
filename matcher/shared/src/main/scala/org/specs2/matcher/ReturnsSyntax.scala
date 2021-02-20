package org.specs2
package matcher

import execute.*
import StringMatchers.{given, *}

/**
 * Those definitions help specifying the result messages for matchers
 */
trait ReturnsSyntax extends ExpectationsCreation:

  extension [T : AsResult](t: =>T)
    def returns(m: String): Result =
      (contain(m) ^^ ((_: Result).message))(createExpectable(ResultExecution.execute(AsResult(t))))

    def returnsMatch(m: String) =
      (beMatching(m) ^^ ((_: Result).message))(createExpectable(ResultExecution.execute(AsResult(t))))

    def returnsResult(m: String): Result =
      lazy val r = AsResult(t)
      (contain(m) ^^ { (m: Result) => if r.isSuccess then "success: "+m.message else "failure: "+m.message })(createExpectable(ResultExecution.execute(r)))

object ReturnsSyntax extends ReturnsSyntax with Expectations
