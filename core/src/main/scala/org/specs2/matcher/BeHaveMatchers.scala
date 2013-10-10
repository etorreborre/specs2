package org.specs2
package matcher

/**
 * This trait provides special matchers to be used in expressions like
 * 
 * `1 must be equalTo(1) and not be equalTo(2)
 */
private[specs2]
trait BeHaveMatchers {
  def be = new NeutralMatcher[Any]
  def have = be
  def not = new NotMatcher[Any]
}
/**
 * This special matcher always return a NeutralMatch MatchResult (an implicit Success)
 */
class NeutralMatcher[T] extends Matcher[T] {
  def apply[S <: T](s: Expectable[S]): MatchResult[S] = NeutralMatch(MatchSuccess("ok", "ko", s))
}

/**
 * This special matcher always return a NotMatch MatchResult. It will negate the next match applied to it.
 */
class NotMatcher[T] extends Matcher[T] {
  def apply[S <: T](s: Expectable[S]): MatchResult[S] = NotMatch(MatchSuccess("ok", "ko", s))
}
