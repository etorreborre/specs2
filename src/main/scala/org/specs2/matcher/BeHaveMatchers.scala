package org.specs2.matcher

trait BeHaveMatchers {
  def be[T] = new Matcher[T] {
	def apply[S <: T](s: =>Expectable[S]): MatchResult[S] = {
	  result(true, "ok", "ko", s)
	}
  }
  def have[T] = be[T]
}