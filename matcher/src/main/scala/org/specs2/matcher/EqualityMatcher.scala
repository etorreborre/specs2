package org.specs2.matcher

import org.specs2.execute.*
import org.specs2.matcher.describe.*
import org.specs2.text.NotNullStrings.*
import Result.*

class EqualityMatcher[T : Diffable](t: =>T) extends AdaptableMatcher[T]:
  outer =>

  protected val ko: String => String = identity

  def adapt(f: T => T, okFunction: String => String, koFunction: String => String): EqualityMatcher[T] =
    new EqualityMatcher(f(t)):
      override def apply[S <: T](s: Expectable[S]): Result =
        val checkedValues = s"\n\nChecked values\n  Actual:   '${s.value}'\n  Expected: '$t'"
        super.apply(s.map(f)).updateMessage(_ + checkedValues)

      override protected val ko: String => String =
        koFunction compose outer.ko

  def apply[S <: T](b: Expectable[S]): Result =
    val (actual, expected) = (b.value, t)
    val diff = Diffable.diff(actual, expected)

    failureDetailsFor(actual, expected) match
      case Some(failureDetail) =>
        result(diff.identical, ko(b.describe(diff.render)), failureDetail)

      case None =>
        result(diff.identical, ko(b.describe(diff.render)), expected.notNull, actual.notNull)

  private def failureDetailsFor(actual: Any, expected: Any): Option[Details] =
    (actual.asInstanceOf[Matchable], expected.asInstanceOf[Matchable]) match
      case (e1: Map[?, ?], e2: Map[?, ?]) => Some(FailureMapDetails(e1.toMap[Any, Any], e2.toMap[Any, Any]) )
      case (e1: Set[?], e2: Set[?]) => Some( FailureSetDetails(e1.toSet[Any], e2.toSet[Any]) )
      case (e1: Array[?], e2: Array[?]) => Some( FailureSeqDetails(e1.toSeq, e2.toSeq) )
      case (e1: Traversable[?], e2: Traversable[?]) if foreachIsDefined(e2) => Some( FailureSeqDetails(e1.toSeq, e2.toSeq) )
      case (e1: Traversable[?], e2: Traversable[?]) => None
      case (e1, e2) => None

  private def foreachIsDefined(seq: Traversable[?]): Boolean =
    try { seq.foreach(identity); true }
    catch { case _: Exception => false }

  def expected: T =
    t
