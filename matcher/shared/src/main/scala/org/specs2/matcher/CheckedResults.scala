package org.specs2
package matcher

import text.Sentences._
import execute._, Result._

/**
 * This trait can be used in conjunction with Pattern matchers:
 *
 * List(1, 2) must beLike { case List(a, b) => ok }
 * List(1, 2) must beLike { case List(a, b) => ko("unexpected") }
 */
trait ExpectedResults extends ExpectationsCreation:
  def ok(m: String): Result =
    checkResultFailure(Success(m))

  def ko(m: String): Result =
    checkResultFailure(Failure(m))

  lazy val ok: Result =
    checkResultFailure(Success("ok"))

  lazy val ko: Result =
    checkResultFailure(Failure("ko"))

object ExpectedResults extends ExpectedResults
