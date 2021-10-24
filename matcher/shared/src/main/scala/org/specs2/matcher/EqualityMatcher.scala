package org.specs2.matcher

import org.specs2.execute._
import org.specs2.matcher.describe._
import org.specs2.text.NotNullStrings._

class EqualityMatcher[T : Diffable](t: =>T) extends AdaptableMatcher[T] { outer =>

  protected val ok: String => String = identity
  protected val ko: String => String = identity

  def adapt(f: T => T, okFunction: String => String, koFunction: String => String) = {
    new EqualityMatcher(f(t)) {
      override def apply[S <: T](s: Expectable[S]): MatchResult[S] = {
        val checkedValues = s"\n\nChecked values\n  Actual:   '${s.value}'\n  Expected: '$t'"
        result(super.apply(s.map(f)).updateMessage(_ + checkedValues), s)
      }

      override protected val ok: String => String = okFunction compose outer.ok
      override protected val ko: String => String = koFunction compose outer.ko
    }
  }

  def apply[S <: T](b: Expectable[S]): MatchResult[S] = {
    val (actual, expected) = (b.value, t)
    val diff = Diffable.diff(actual, expected)

    // we make sure that values are not null and then we use the actual equality with `==`
    // to determine if values are equal. In principle diff.identical` should return the same value
    // as equals but if it is not the case `equals` should take precedence.
    // The only exception is arrays where we use the Diffable instance to do an element by element
    // comparison where == only does an array reference comparison.
    // Note that if diff.identical differs from equals, it is still possible to intercept the MatchResult and
    // inspect the detail field to get the difference according to the Diffable instance
    val isEqual =
          (actual, expected) match {
            case (a1: Array[?], a2: Array[?]) => diff.identical
            case _ => actual == expected
          }

    failureDetailsFor(actual, expected) match {
      case Some(failureDetail) =>
        result(isEqual,
               ok(s"${b.description} == '${expected.notNull}'"),
               ko(b.describe(diff.render)), b, failureDetail)

      case None =>
        result(isEqual,
               ok(s"${b.description} == '${expected.notNull}'"),
               ko(b.describe(diff.render)), b, expected.notNull, actual.notNull)
    }
  }

  private def failureDetailsFor(actual: Any, expected: Any): Option[Details] =
    (actual, expected) match {
      case (e1: Map[?, ?], e2: Map[?, ?]) => Some(FailureMapDetails(e1.toMap[Any, Any], e2.toMap[Any, Any]))
      case (e1: Set[?], e2: Set[?]) => Some(FailureSetDetails(e1.toSet[Any], e2.toSet[Any]))
      case (e1: Array[?], e2: Array[?]) => Some(FailureSeqDetails(e1.toSeq, e2.toSeq))
      case (e1: Traversable[?], e2: Traversable[?]) if foreachIsDefined(e2) => Some(FailureSeqDetails(e1.toSeq.seq, e2.toSeq.seq))
      case (e1: Traversable[?], e2: Traversable[?]) => None
      case (e1, e2) => None
    }

  private def foreachIsDefined(seq: Traversable[?]): Boolean =
    try { seq.foreach(identity); true }
    catch { case _: Exception => false }

  def expected = t
}
