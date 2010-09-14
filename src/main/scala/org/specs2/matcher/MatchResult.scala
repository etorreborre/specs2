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
case class MatchSuccess[T](val okMessage: String, val koMessage: String, e: Expectable[T]) extends MatchResult[T] {
  val expectable: Expectable[T] = e
  def not = MatchFailure(okMessage, koMessage, e)
  def or(m: =>MatchResult[T]): MatchResult[T] = this
  def and(m: =>MatchResult[T]): MatchResult[T] = m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" and "+okMessage, ko+" and "+okMessage, e)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, e)
  }
  def toResult = Success(okMessage)
}
case class MatchFailure[T](val okMessage: String, val koMessage: String, e: Expectable[T]) extends MatchResult[T] {
  val expectable: Expectable[T] = e
  def not = MatchSuccess(okMessage, koMessage, e)
  def or(m: =>MatchResult[T]): MatchResult[T]  = m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, e)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+" and "+okMessage, e)
  } 
  def and(m: =>MatchResult[T]): MatchResult[T] =  m match {
	case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, e)
	case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, e)
  } 
  def toResult = Failure(koMessage)
}