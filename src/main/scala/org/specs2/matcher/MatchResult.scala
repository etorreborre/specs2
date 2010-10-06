package org.specs2
package matcher
import execute._
import scalaz.Functor
import Expectable._

sealed trait MatchResult[T] {
  val expectable: Expectable[T]
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S]
  def not: MatchResult[T]
  def or(m: =>MatchResult[T]): MatchResult[T]
  def or(m: Matcher[T]): MatchResult[T] = or(expectable.applyMatcher(m))
  def and(m: =>MatchResult[T]): MatchResult[T]
  def and(m: Matcher[T]): MatchResult[T] = and(expectable.applyMatcher(m))
  def toResult: Result
}
object MatchResult {
  implicit def MatchResultFunctor[T](m: MatchResult[T]) = new Functor[MatchResult] {
	  def fmap[A, B](m: MatchResult[A], f: A => B) = m match {
	    case success: MatchSuccess[_] => success.fmap(success, f)
	    case failure: MatchFailure[_] => failure.fmap(failure, f)
	    case skip: MatchSkip[_] => skip.fmap(skip, f)
	  }
  }	
	
  implicit def MatchSuccessFunctor[T](m: MatchSuccess[T]): Functor[MatchSuccess] = new Functor[MatchSuccess] {
	  def fmap[A, B](m: MatchSuccess[A], f: A => B) = new MatchSuccess(m.okMessage, m.koMessage, m.expectable.fmap(m.expectable, f))
  }	
  implicit def MatchFailureFunctor[T](m: MatchFailure[T]): Functor[MatchFailure] = new Functor[MatchFailure] {
	  def fmap[A, B](m: MatchFailure[A], f: A => B) = new MatchFailure(m.okMessage, m.koMessage, m.expectable.fmap(m.expectable, f))
  }	
  implicit def MatchSkipFunctor[T](m: MatchSkip[T]): Functor[MatchSkip] = new Functor[MatchSkip] {
	  def fmap[A, B](m: MatchSkip[A], f: A => B) = new MatchSkip(m.message, m.expectable.fmap(m.expectable, f))
  }	
}
case class MatchSuccess[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSuccess(okMessage, koMessage, f(expectable))
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
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchFailure(okMessage, koMessage, f(expectable))
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
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSkip(message, f(expectable))
  def toResult = Skipped(message)
  def not = MatchSuccess("ok", "ko", expectable)
  def or(m: =>MatchResult[T]) = this
  def and(m: =>MatchResult[T]) = this
}