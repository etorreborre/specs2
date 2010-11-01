package org.specs2
package matcher

import scalaz.Functor
import execute._
import Expectable._

sealed trait MatchResult[+T] {
  val expectable: Expectable[T]
  
  protected[specs2] def evaluate[S >: T]: MatchResult[S] = this
  def apply(m: Matcher[T]): MatchResult[T]
  def not: MatchResult[T]
  def or[S >: T](m: =>MatchResult[S]): MatchResult[S] = new OrMatch(this, m).evaluate
  def and[S >: T](m: =>MatchResult[S]): MatchResult[S] = AndMatch(this, m).evaluate
  def or(m: Matcher[T]): MatchResult[T] = or(expectable.applyMatcher(m))
  def and(m: Matcher[T]): MatchResult[T] = and(expectable.applyMatcher(m))
  def be(m: Matcher[T]) = apply(m)
  def have(m: Matcher[T]) = apply(m)
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S]
  def toResult: Result = evaluate.toResult
}
case class MatchSuccess[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSuccess(okMessage, koMessage, f(expectable))
  override def toResult = Success(okMessage)
  def not: MatchResult[T] = MatchFailure(koMessage, okMessage, expectable)
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
}
case class MatchFailure[T](okMessage: String, koMessage: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchFailure(okMessage, koMessage, f(expectable))
  override def toResult = Failure(koMessage)
  def not: MatchResult[T] = MatchSuccess(okMessage, koMessage, expectable)
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
}
case class MatchSkip[T](message: String, expectable: Expectable[T]) extends MatchResult[T] {
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = MatchSkip(message, f(expectable))
  def not: MatchResult[T] = this
  def apply(matcher: Matcher[T]): MatchResult[T] = expectable.applyMatcher(matcher)
  override def toResult = Skipped(message)
}
case class NotMatch[T](m: MatchResult[T]) extends MatchResult[T] {
  val expectable = m.expectable
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = NotMatch(m compose f)
  override def evaluate[S >: T] = m
  def not: MatchResult[T] = NeutralMatch(m)
  def apply(matcher: Matcher[T]): MatchResult[T] = m.apply(matcher.not)
}
case class NeutralMatch[T](m: MatchResult[T]) extends MatchResult[T] {
  val expectable = m.expectable
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = NeutralMatch(m compose f)
  override def evaluate[S >: T] = m
  def not: MatchResult[T] = NotMatch(m)
  def apply(matcher: Matcher[T]): MatchResult[T] = m.apply(matcher)
}
case class AndMatch[T](m1: MatchResult[T], m2: MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  override def evaluate[S >: T] = 
    (m1, m2) match {
    case (_, NeutralMatch(_)) => AndMatch(m1, MatchSkip("", expectable))
    case (NeutralMatch(_), _) => AndMatch(m2, MatchSkip("", expectable))
    case (NotMatch(_), NotMatch(_)) => AndNotMatch(m1.evaluate, m2.evaluate) 
    case (_, NotMatch(_)) => AndNotMatch(m1, MatchSkip("", expectable))
    case (NotMatch(_), _) => AndMatch(m1.evaluate, m2).evaluate
    case (MatchSuccess(_, _, _), MatchFailure(_, _, _)) => m2 
    case (MatchFailure(_, _, _), MatchSuccess(_, _, _)) => m1 
    case (MatchSuccess(_, _, _), _) => m1 
    case (_, MatchSuccess(_, _, _)) => m2 
    case (_, _) => m1 
  }
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = AndMatch(m1 compose f, m2 compose f)
  def not: MatchResult[T] = new OrMatch(m1.not, m2.not).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 and m2.apply(matcher)
}
case class AndNotMatch[T](m1: MatchResult[T], m2: MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  override def evaluate[S >: T] = m1 and m2.not
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = AndNotMatch(m1 compose f, m2 compose f)
  def not: MatchResult[T] = new OrMatch(m1.not, m2).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 and m2.apply(matcher.not)
}
class OrMatch[T](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  val expectable = m1.expectable
  def m1 = first
  def m2 = second
  override def evaluate[S >: T] = {
    m1 match {
      case MatchSuccess(_, _, _) => new OrMatch(m1, MatchSkip("", expectable))
      case _ => {
        (m1, m2) match {
          case (_, NeutralMatch(_)) => new OrMatch(m1, MatchSkip("", expectable))
          case (NeutralMatch(_), _) => new OrMatch(m2, MatchSkip("", expectable))
          case (NotMatch(_), NotMatch(_)) => new OrNotMatch(m1.evaluate, m2)
          case (_, NotMatch(_)) => new OrNotMatch(m1, m2) 
          case (NotMatch(_), _) => new OrMatch(m1.evaluate, m2).evaluate
          case (_, MatchSuccess(_, _, _)) => m2
          case (_, _) => m1 
        }
      }
    }
  }
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = new OrMatch(m1 compose f, m2 compose f)
  def not: MatchResult[T] = AndMatch(m1.not, m2.not).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 or m2.apply(matcher)
  override def toResult = m1.toResult or m2.toResult
}
class OrNotMatch[T](first: MatchResult[T], second: =>MatchResult[T]) extends MatchResult[T] {
  def m1 = first
  def m2 = second
  val expectable = m1.expectable
  override def evaluate[S >: T] = m1 or m2.not
  def compose[S](f: Expectable[T] => Expectable[S]): MatchResult[S] = new OrNotMatch(m1 compose f, m2 compose f)
  def not: MatchResult[T] = AndMatch(m1.not, m2).evaluate
  def apply(matcher: Matcher[T]): MatchResult[T] = m1 or evaluate.apply(matcher.not)
}

private[specs2]
object MatchResult {
  implicit def MatchResultFunctor[T](m: MatchResult[T]) = new Functor[MatchResult] {
    def fmap[A, B](m: MatchResult[A], f: A => B) = m match {
      case success: MatchSuccess[_] => success.fmap(success, f)
      case failure: MatchFailure[_] => failure.fmap(failure, f)
      case skip: MatchSkip[_] => skip.fmap(skip, f)
      case neg: NotMatch[_] => neg.fmap(neg, f)
      case neutral: NeutralMatch[_] => neutral.fmap(neutral, f)
      case and: AndMatch[_] => and.fmap(and, f)
      case andnot: AndNotMatch[_] => andnot.fmap(andnot, f)
      case or: OrMatch[_] => or.fmap(or, f)
      case ornot: OrNotMatch[_] => ornot.fmap(ornot, f)
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
  implicit def NotMatchFunctor[T](n: NotMatch[T]): Functor[NotMatch] = new Functor[NotMatch] {
    def fmap[A, B](n: NotMatch[A], f: A => B) = new NotMatch(n.m.fmap(n.m, f))
  } 
  implicit def NeutralMatchFunctor[T](n: NeutralMatch[T]): Functor[NeutralMatch] = new Functor[NeutralMatch] {
    def fmap[A, B](n: NeutralMatch[A], f: A => B) = new NeutralMatch(n.m.fmap(n.m, f))
  } 
  implicit def AndMatchFunctor[T](m: AndMatch[T]): Functor[AndMatch] = new Functor[AndMatch] {
    def fmap[A, B](m: AndMatch[A], f: A => B) = new AndMatch(m.m1.fmap(m.m1, f), m.m2.fmap(m.m2, f))
  } 
  implicit def AndNotMatchFunctor[T](m: AndNotMatch[T]): Functor[AndNotMatch] = new Functor[AndNotMatch] {
    def fmap[A, B](m: AndNotMatch[A], f: A => B) = new AndNotMatch(m.m1.fmap(m.m1, f), m.m2.fmap(m.m2, f))
  } 
  implicit def OrMatchFunctor[T](m: OrMatch[T]): Functor[OrMatch] = new Functor[OrMatch] {
    def fmap[A, B](m: OrMatch[A], f: A => B) = new OrMatch(m.m1.fmap(m.m1, f), m.m2.fmap(m.m2, f))
  } 
  implicit def OrNotMatchFunctor[T](m: OrNotMatch[T]): Functor[OrNotMatch] = new Functor[OrNotMatch] {
    def fmap[A, B](m: OrNotMatch[A], f: A => B) = new OrNotMatch(m.m1.fmap(m.m1, f), m.m2.fmap(m.m2, f))
  } 
}
