package org.specs2
package matcher

/**
 * This trait can be used in conjonction with Pattern matchers:
 *
 * List(1, 2) must be like { case List(a, b) => ok }
 * List(1, 2) must be like { case List(a, b) => ko("unexpected") }
 */
trait StandardMatchResults {
  def ok(m: String) = new MatchSuccess(m, "ko", Expectable(None))
  def ko(m: String) = new MatchFailure("ok", m, Expectable(None))
  val ok = new MatchSuccess("ok", "ko", Expectable(None))
  val ko = new MatchFailure("ok", "ko", Expectable(None))
}