package org.specs2
package matcher

import execute.*
import text.Whitespace.*
import StringMatchers.{given, *}

/** Those definitions help specifying the result messages for matchers
  */
trait ReturnsSyntax extends ExpectationsCreation:

  extension [T: AsResult](t: =>T)
    infix def returns(m: String): Result =
      lazy val r = AsResult.safely(t)
      lazy val resultMessage = r.message
      contain(m.showWhitespaces).setMessage(
        s"${resultMessage.showWhitespaces}\n does not return\n${m.showWhitespaces}"
      )(createExpectable(resultMessage.showWhitespaces))

  infix def returnsMatch(m: String) =
    lazy val r = AsResult.safely(t)
    (beMatching(m) ^^ ((_: Result).message))(createExpectable(r))

  infix def returnsResult(m: String): Result =
    lazy val r = AsResult.safely(t)
    (contain(m) ^^ { (m: Result) =>
      if r.isSuccess then "success: " + m.message
      else "failure: " + m.message
    })(createExpectable(r))

object ReturnsSyntax extends ReturnsSyntax with Expectations
