package org.specs2.matcher

import org.specs2.execute._
import org.specs2.matcher.describe._
import org.specs2.text.NotNullStrings._
import Result._

class EqualityMatcher[T : Diffable](t: =>T) extends AdaptableMatcher[T]:
  outer =>

  lazy val expected: T =
    t

  protected val ko: String => String = identity

  def adapt(f: T => T, okFunction: String => String, koFunction: String => String): EqualityMatcher[T] =
    new EqualityMatcher(f(expected)):
      override def apply[S <: T](s: Expectable[S]): Result =
        val checkedValues = s"\n\nChecked values\n  Actual:   '${s.value}'\n  Expected: '$expected'"
        super.apply(s.map(f)).updateMessage(_ + checkedValues)

      override protected val ko: String => String =
        koFunction compose outer.ko

  def apply[S <: T](b: Expectable[S]): Result =
    val actual = b.value
    val diff = Diffable.diff(actual, expected)

    failureDetailsFor(actual, expected) match
      case Some(failureDetail) =>
        result(diff.identical, ko(b.describe(diff.render)), failureDetail)

      case None =>
        result(diff.identical, ko(b.describe(diff.render)), expected.notNull, actual.notNull)

  override def not: Matcher[T] =
    new Matcher[T]:
      def apply[S <: T](b: Expectable[S]): Result =
        result(!outer(b).isSuccess, b.description + " === " +outer.expected)

  private def failureDetailsFor(actual: Any, expected: Any): Option[Details] =
    (actual, expected) match
      case (e1: Map[_, _], e2: Map[_, _]) => Some(FailureMapDetails(e1.toMap[Any, Any], e2.toMap[Any, Any]) )
      case (e1: Set[_], e2: Set[_]) => Some( FailureSetDetails(e1.toSet[Any], e2.toSet[Any]) )
      case (e1: Array[_], e2: Array[_]) => Some( FailureSeqDetails(e1.toSeq, e2.toSeq) )
      case (e1: Traversable[_], e2: Traversable[_]) if foreachIsDefined(e2) => Some( FailureSeqDetails(e1.toSeq, e2.toSeq) )
      case (e1: Traversable[_], e2: Traversable[_]) => None
      case (e1, e2) => None

  private def foreachIsDefined(seq: Traversable[_]): Boolean =
    try { seq.foreach(identity); true }
    catch { case _: Exception => false }
