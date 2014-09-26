package org.specs2
package matcher

/**
 * This trait can be used in conjonction with Pattern matchers:
 *
 * List(1, 2) must be like { case List(a, b) => ok }
 * List(1, 2) must be like { case List(a, b) => ko("unexpected") }
 */
trait StandardMatchResults extends ExpectationsCreation {
  def ok(m: String): MatchResult[Any] =
    MatchSuccess(m, "ko", createExpectable(None))

  def ko(m: String): MatchResult[Any] =
    MatchFailure("ok", m, createExpectable(None))

  lazy val ok: MatchResult[Any] =
    MatchSuccess("ok", "ko", createExpectable(None))

  lazy val ko: MatchResult[Any] =
    MatchFailure("ok", "ko", createExpectable(None))
}

object StandardMatchResults extends StandardMatchResults