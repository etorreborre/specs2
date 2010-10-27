package org.specs2
package matcher
import execute._
import scalaz.Functor
import Expectable._

sealed trait MatchResult[+T] {
  val expectable: Expectable[T]
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S]
  def not: MatchResult[T]
  def or[S >: T](m: =>MatchResult[S]): MatchResult[S]
  def and[S >: T](m: =>MatchResult[S]): MatchResult[S]
  def or(m: Matcher[T]): MatchResult[T] = or(expectable.applyMatcher(m))
  def and(m: Matcher[T]): MatchResult[T] = and(expectable.applyMatcher(m))
  def be(m: Matcher[T]) = and(m)
  def have(m: Matcher[T]) = and(m)
  def toResult: Result
}
object MatchResult {
  implicit def MatchResultFunctor[T](m: MatchResult[T]) = new Functor[MatchResult] {
	  def fmap[A, B](m: MatchResult[A], f: A => B) = m match {
	    case success: MatchSuccess[_] => success.fmap(success, f)
	    case failure: MatchFailure[_] => failure.fmap(failure, f)
	    case skip: MatchSkip[_] => skip.fmap(skip, f)
      case neg: NegatedMatch[_] => neg.fmap(neg, f)
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
  implicit def NegatedMatchFunctor[T](m: NegatedMatch[T]): Functor[NegatedMatch] = new Functor[NegatedMatch] {
    def fmap[A, B](m: NegatedMatch[A], f: A => B) = new NegatedMatch(m.expectable.fmap(m.expectable, f))
  } 
}
case class MatchSuccess[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSuccess(okMessage, koMessage, f(expectable))
  def not = MatchFailure(koMessage, okMessage, expectable)
  def or[S >: T](m: =>MatchResult[S]) = this
  def and[S >: T](m: =>MatchResult[S]) = m match {
	  case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" and "+okMessage, ko+" and "+okMessage, expectable)
	  case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, expectable)
	  case r => r
  }
  def toResult = Success(okMessage)
}
case class MatchFailure[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchFailure(okMessage, koMessage, f(expectable))
  def not = MatchSuccess(koMessage, okMessage, expectable)
  def or[S >: T](m: =>MatchResult[S])  = m match {
	  case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, expectable)
	  case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+" and "+okMessage, expectable)
    case r => this
  } 
  def and[S >: T](m: =>MatchResult[S]) =  m match {
	  case MatchSuccess(ok, ko, e) => MatchSuccess(ok+" but "+koMessage, ko, expectable)
	  case MatchFailure(ok, ko, e) => MatchFailure(ko+" and "+koMessage, ok+ " and "+okMessage, expectable)
    case r => this
  } 
  def toResult = Failure(koMessage)
}
case class MatchSkip[T](message: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSkip(message, f(expectable))
  def toResult = Skipped(message)
  def not = MatchSuccess("ok", "ko", expectable)
  def or[S >: T](m: =>MatchResult[S]) = this
  def and[S >: T](m: =>MatchResult[S]) = this
}
case class NegatedMatch[T](expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = NegatedMatch(f(expectable))
  def not: MatchResult[T] = this
  def or[S >: T](m: =>MatchResult[S]) = this
  def and[S >: T](m: =>MatchResult[S]) = m.not
  def toResult: Result = Success("not")
}