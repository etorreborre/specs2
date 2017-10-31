package org.specs2
package matcher

import text.Sentences._

/**
 * This trait can be used in conjunction with Pattern matchers:
 *
 * List(1, 2) must be like { case List(a, b) => ok }
 * List(1, 2) must be like { case List(a, b) => ko("unexpected") }
 */
trait StandardMatchResults extends ExpectationsCreation {
  def ok(m: String): MatchResult[Any] =
    checkFailure(Matcher.result(true, m, negateSentence(m), createExpectable(None)))

  def ko(m: String): MatchResult[Any] =
    checkFailure(Matcher.result(false, negateSentence(m), m, createExpectable(None)))

  lazy val ok: MatchResult[Any] =
    checkFailure(Matcher.result(true, "ok", "ko", createExpectable(None)))

  lazy val ko: MatchResult[Any] =
    checkFailure(Matcher.result(false, "ok", "ko", createExpectable(None)))
}

object StandardMatchResults extends StandardMatchResults