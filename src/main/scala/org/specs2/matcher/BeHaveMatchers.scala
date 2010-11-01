package org.specs2.matcher

private[specs2]
trait BeHaveMatchers {
  def be = new NeutralMatcher[Any]
  def have = be
  def not = new NotMatcher[Any] 
}
class NeutralMatcher[T] extends Matcher[T] {
  def apply[S <: T](s: =>Expectable[S]): MatchResult[S] = NeutralMatch(MatchSuccess("ok", "ko", s))
}

class NotMatcher[T] extends Matcher[T] {
  def apply[S <: T](s: =>Expectable[S]): MatchResult[S] = NotMatch(MatchSuccess("ok", "ko", s))
}
