package org.specs2
package matcher

trait StandardMatchResults {
  def ok(m: String) = new MatchSuccess(m, "ko", Expectable(None))
  def ko(m: String) = new MatchFailure("ok", m, Expectable(None))
  val ok = new MatchSuccess("ok", "ko", Expectable(None))
  val ko = new MatchFailure("ok", "ko", Expectable(None))
}