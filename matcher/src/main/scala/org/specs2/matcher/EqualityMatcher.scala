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
    val universalDiff = Diffable.fallbackDiffable.diff(actual, expected)
    val specificDiff = Diffable.diff(actual, expected)
    val failureMessage = ko(b.describe(universalDiff.render+"\n"+specificDiff.render))
    result(specificDiff.identical, failureMessage, expected.notNull, actual.notNull)

  def expected: T =
    t
