package org.specs2.matcher

trait BeHaveMatchers {
  def be[T] = new Matcher[T] {
	def apply[S <: T : Expectable](s: =>S): MatchResult[S] = {
	  result(true, "ok", "ko")
	}
  }
  def have[T] = be[T]
}