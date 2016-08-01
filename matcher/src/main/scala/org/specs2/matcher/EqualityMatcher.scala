package org.specs2.matcher

import org.specs2.execute._
import org.specs2.matcher.describe._
import org.specs2.text.NotNullStrings._

import scala.collection.{GenTraversable, GenTraversableOnce}

class EqualityMatcher[T : Diffable](t: =>T, equality: (T, T) => Boolean = (t1:T, t2:T) => t1 == t2) extends AdaptableMatcher[T] { outer =>

  protected val ok: String => String = identity
  protected val ko: String => String = identity

  def adapt(f: T => T, okFunction: String => String, koFunction: String => String) = {
    new EqualityMatcher(f(t), equality) {
      override def apply[S <: T](s: Expectable[S]): MatchResult[S] = {
        val originalValues = s"\nOriginal values\n  Expected: '$t'\n  Actual  : '${s.value}'"
        result(super.apply(s.map(f)).updateMessage(_ + originalValues), s)
      }

      override protected val ok: String => String = okFunction compose outer.ok
      override protected val ko: String => String = koFunction compose outer.ko
    }
  }

  /**
   * we perform 2 kinds of check, depending on the elements to compare
   *
   *  - unordered sequences (maps, sets) are being compared with a matching algorithm
   *  - arrays are being compared with the deep equality and a matching algorithm is used for missing elements
   *  - sequences are being compared with the regular equality and a matching algorithm is used for missing elements
   *  - other objects are being compared using the regular equality
   *
   * @return a MatchResult describing the outcome of the match
   */
  def apply[S <: T](b: Expectable[S]): MatchResult[S] = {
    val (actual, expected) = (b.value, t)
    val isEqual = checkEquality(actual, expected)
    val diff = Diffable.diff(actual, expected)

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

  private def checkEquality(actual: Any, expected: Any): Boolean =
    (actual, expected) match {
      case (e1: Array[_], e2: Array[_]) => e1.deep == e2.deep
      case (e1, e2) => e1 == e2
    }

  private def failureDetailsFor(actual: Any, expected: Any): Option[Details] =
    (actual, expected) match {
      case (e1: Map[_, _], e2: Map[_, _]) => Some( FailureMapDetails(e1.toMap[Any, Any], e2.toMap[Any, Any]) )
      case (e1: Set[_], e2: Set[_]) => Some( FailureSetDetails(e1.toSet[Any], e2.toSet[Any]) )
      case (e1: Array[_], e2: Array[_]) => Some( FailureSeqDetails(e1.toSeq, e2.toSeq) )
      case (e1: GenTraversable[_], e2: GenTraversable[_]) if foreachIsDefined(e2) => Some( FailureSeqDetails(e1.toSeq.seq, e2.toSeq.seq) )
      case (e1: GenTraversable[_], e2: GenTraversable[_]) => None
      case (e1, e2) => None
    }

  private def foreachIsDefined(seq: GenTraversableOnce[_]): Boolean =
    try { seq.foreach(identity); true }
    catch { case _: Exception => false }

  def expected = t
}
