package org.specs2
package matcher
import execute._

sealed trait MatchResult[T] {
  val expectable: Expectable[T]
  def not: MatchResult[T]
  def or(m: =>MatchResult[T]): MatchResult[T]
  def or(m: Matcher[T]): MatchResult[T] = or(expectable.applyMatcher(m))
  def and(m: =>MatchResult[T]): MatchResult[T]
  def and(m: Matcher[T]): MatchResult[T] = and(expectable.applyMatcher(m))
  def toResult: Result
}
case class MatchSuccess[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def not = MatchFailure(okMessage, koMessage, expectable)
  def or(m: =>MatchResult[T]): MatchResult[T] = this
  def and(m: =>MatchResult[T]): MatchResult[T] = m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" and "+okMessage, ko+" and "+okMessage, expectable)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, expectable)
	case r @ MatchSkip(_, _) => r
  }
  def toResult = Success(okMessage)
}
case class MatchFailure[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def not = MatchSuccess(okMessage, koMessage, expectable)
  def or(m: =>MatchResult[T]): MatchResult[T]  = m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, expectable)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+" and "+okMessage, expectable)
	case MatchSkip(_, _) => this
  } 
  def and(m: =>MatchResult[T]): MatchResult[T] =  m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, expectable)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, expectable)
	case MatchSkip(_, _) => this
  } 
  def toResult = Failure(koMessage)
}
case class MatchSkip[T](message: String, expectable: Expectable[T]) extends MatchResult[T] {
  def toResult = Skipped(message)
  def not = MatchSuccess("ok", "ko", expectable)
  def or(m: =>MatchResult[T]) = this
  def and(m: =>MatchResult[T]) = this
}