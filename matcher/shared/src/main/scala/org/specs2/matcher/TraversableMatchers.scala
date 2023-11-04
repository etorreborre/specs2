package org.specs2
package matcher

import control.*
import collection.*
import collection.Seqx.*
import text.Regexes.*
import text.Plural.*
import text.NotNullStrings.*

import scala.collection.Traversable
import scala.annotation.tailrec
import execute.*
import ValueChecks.{given, *}
import StringMatchers.{given, *}
import describe.Diffable
import Result.*

/** Matchers for traversables
  */
trait TraversableMatchers extends TraversableBaseMatchers with TraversableBaseMatchersLowImplicits with NumberOfTimes

object TraversableMatchers extends TraversableMatchers

trait TraversableBaseMatchers:
  outer =>

  /** ELEMENTS MATCHERS
    */
  def contain[T](check: ValueCheck[T]): ContainWithResult[T] =
    new ContainWithResult(check)

  /** COLLECTION MATCHERS
    */
  def contain[T](cm: ContainWithResultSeq[T]): ContainWithResultSeq[T] = cm

  def allOf[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] =
    new ContainWithResultSeq(checks).atLeast.onDistinctValues(false)
  def eachOf[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def exactly[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).exactly
  def atLeast[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def atMost[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atMost

  /** match if a traversable contains all the elements of seq (and maybe more) */
  def containAllOf[T: Diffable](seq: Seq[T]) =
    contain(atLeast(seq.map(v => valueIsTypedValueCheck(v))*))

  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]): ContainWithResult[T] =
    contain(new BeOneOf(seq))

  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](s: =>String): Matcher[Traversable[T]] =
    containPattern(s.regexPart)

  /** match if traversable contains (x matches p) */
  def containPattern[T, S: MatchingExpression](s: S): Matcher[Traversable[T]] =
    contain(atLeast(ValueChecks.matcherIsValueCheck(beMatching(s)))) ^^ (_.map(_.toString))

  /** does a containAll comparison in both ways */
  def containTheSameElementsAs[T](
      seq: Seq[T],
      equality: (T, T) => Boolean = (_: T) == (_: T)
  ): Matcher[Traversable[T]] =
    new Matcher[Traversable[T]]:

      def apply[S <: Traversable[T]](t: Expectable[S]) =
        val missing = seq.difference(t.value.toSeq, equality)
        val added = t.value.toSeq.difference(seq, equality)
        def message(diffs: scala.collection.Seq[?], msg: String) =
          if diffs.isEmpty then "" else diffs.mkString("\n  " + msg + ": ", ", ", "")

        result(
          missing.isEmpty && added.isEmpty,
          t.description + message(missing, "is missing") + message(added, "must not contain")
        )

  /** SIZE MATCHERS
    */

  /** match if there is a way to size T */
  def haveSize[T: Sized](n: Int): SizedMatcher[T] = new SizedMatcher[T](n, "size")

  /** alias for haveSize */
  def size[T: Sized](n: Int): SizedMatcher[T] = haveSize[T](n)

  /** alias for haveSize */
  def haveLength[T: Sized](n: Int): SizedMatcher[T] = new SizedMatcher[T](n, "length")

  /** alias for haveSize */
  def length[T: Sized](n: Int): SizedMatcher[T] = haveLength[T](n)

  /** match if there is a way to size T */
  def haveSize[T: Sized](check: ValueCheck[Int]): SizedCheckedMatcher[T] = new SizedCheckedMatcher[T](check, "size")

  /** alias for haveSize */
  def size[T: Sized](check: ValueCheck[Int]): SizedCheckedMatcher[T] = haveSize[T](check)

  /** alias for haveSize */
  def haveLength[T: Sized](check: ValueCheck[Int]): SizedCheckedMatcher[T] = new SizedCheckedMatcher[T](check, "length")

  /** alias for haveSize */
  def length[T: Sized](check: ValueCheck[Int]): SizedCheckedMatcher[T] = haveLength[T](check)

  /** @return a matcher checking if the elements are ordered */
  def beSorted[T: Ordering]: OrderingMatcher[T] = new OrderingMatcher[T]

  /** alias for beSorted */
  def sorted[T: Ordering]: OrderingMatcher[T] = beSorted[T]

  /** Additional contain methods using to avoid automatic tuple conversions
    */

  // 2 to 5
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T]): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3)
  )
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T]): ContainWithResultSeq[T] =
    contain(allOf(t1, t2, t3, t4))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5))
  // 6 to 10
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
  // 11 to 15
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
  // 16 to 20
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
  )
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T],
      t18: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
  )
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T],
      t18: ValueCheck[T],
      t19: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
  )
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T],
      t18: ValueCheck[T],
      t19: ValueCheck[T],
      t20: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)
  )
  // 21 to 22
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T],
      t18: ValueCheck[T],
      t19: ValueCheck[T],
      t20: ValueCheck[T],
      t21: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t21, t21)
  )
  def contain[T](
      t1: ValueCheck[T],
      t2: ValueCheck[T],
      t3: ValueCheck[T],
      t4: ValueCheck[T],
      t5: ValueCheck[T],
      t6: ValueCheck[T],
      t7: ValueCheck[T],
      t8: ValueCheck[T],
      t9: ValueCheck[T],
      t10: ValueCheck[T],
      t11: ValueCheck[T],
      t12: ValueCheck[T],
      t13: ValueCheck[T],
      t14: ValueCheck[T],
      t15: ValueCheck[T],
      t16: ValueCheck[T],
      t17: ValueCheck[T],
      t18: ValueCheck[T],
      t19: ValueCheck[T],
      t20: ValueCheck[T],
      t21: ValueCheck[T],
      t22: ValueCheck[T]
  ): ContainWithResultSeq[T] = contain(
    allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t21, t22)
  )

private[specs2] trait TraversableBaseMatchersLowImplicits extends ValueChecksLowImplicits:

  given seqToValueChecks[T](using to: T => ValueCheck[T]): Conversion[Seq[T], Seq[ValueCheck[T]]] with
    def apply(seq: Seq[T]): Seq[ValueCheck[T]] =
      seq.map(to)

  given matchersToValueChecks[T]: Conversion[Seq[Matcher[T]], Seq[ValueCheck[T]]] with
    def apply(seq: Seq[Matcher[T]]): Seq[ValueCheck[T]] =
      seq.map(matcherIsValueCheck[T])

class SizedMatcher[T: Sized](n: Int, sizeWord: String) extends Matcher[T]:
  def apply[S <: T](traversable: Expectable[S]) =
    val s = implicitly[Sized[T]]
    val valueSize = s.size(traversable.value)
    result(valueSize == n, s"'${traversable.description}' doesn't have $sizeWord $n but $sizeWord $valueSize")

class SizedCheckedMatcher[T: Sized](check: ValueCheck[Int], sizeWord: String) extends Matcher[T]:
  def apply[S <: T](traversable: Expectable[S]) =
    val s = implicitly[Sized[T]]
    val valueSize = s.size(traversable.value)
    val checked = check.check(valueSize)
    result(checked.isSuccess, traversable.description + " doesn't have the right " + sizeWord + ": " + checked.message)

class OrderingMatcher[T: Ordering] extends Matcher[Seq[T]]:
  def apply[S <: Seq[T]](traversable: Expectable[S]) =
    result(traversable.value == traversable.value.sorted, traversable.description + " is not sorted")

import control.NumberOfTimes.*
import text.Plural.*

case class ContainWithResult[T](
    check: ValueCheck[T],
    timesMin: Option[Times] = Some(Times(1)),
    timesMax: Option[Times] = None,
    checkAll: Boolean = true,
    negate: Boolean = false
) extends Matcher[Traversable[T]]:

  def apply[S <: Traversable[T]](t: Expectable[S]) =
    val seq = Vector(t.value.toSeq*)

    // stop after the first failure if !checkAll
    val (successes, failures) = seq.foldLeft((Seq[Result](), Seq[Result]())) { (res, cur) =>
      val (ss, fs) = res
      if !checkAll && fs.nonEmpty then res
      else
        check.check(cur) match
          case e: Error         => throw new ErrorException(e)
          case r if r.isSuccess => (ss :+ r, fs)
          case r                => (ss, fs :+ r)
    }

    val r = failures.collect { case s: Skipped => s }.headOption.getOrElse {
      val (okMessage, koMessage) = messages(t.description, successes, failures)
      val details: Details =
        failures.collect { case Failure(_, _, _, d) if d != NoDetails => d }.headOption.getOrElse(NoDetails)

      (timesMin, timesMax) match
        case (None, None) => Result.result(successes.size == seq.size, koMessage, details)
        case (Some(Times(min)), None) =>
          val message = koMessage + s"""\nNumber of successful matches: ${successes.size}. Expected: at least $min"""
          Result.result(successes.size >= min, message, details)
        case (None, Some(Times(max))) =>
          val message = koMessage + s"""\nNumber of successful matches: ${successes.size}. Expected: at most $max"""
          Result.result(successes.size <= max, message, details)
        case (Some(Times(min)), Some(Times(max))) =>
          val expected =
            if min == max then s"exactly $min" else if min == 1 then s"at most $max" else s"between $min and $max"
          val message = koMessage + s"""\nNumber of successful matches: ${successes.size}. Expected: $expected"""
          Result.result(successes.size >= min && successes.size <= max, message, details)
    }

    if negate then Result.result(!r.isSuccess, "Expectation failed:\n" + r.message)
    else r

  def atLeastOnce: ContainWithResult[T] = atLeast(1.times)
  def atLeast(times: Times): ContainWithResult[T] = copy(timesMin = Option(times))
  def atLeast(n: Int): ContainWithResult[T] = atLeast(Times(n))

  def atMostOnce: ContainWithResult[T] = atMost(1.times)
  def atMost(times: Times): ContainWithResult[T] = copy(timesMax = Option(times))
  def atMost(n: Int): ContainWithResult[T] = atMost(Times(n))

  def between(min: Times, max: Times): ContainWithResult[T] = atLeast(min).atMost(max)
  def between(min: Int, max: Int): ContainWithResult[T] = between(Times(min), Times(max))

  def exactly(times: Times): ContainWithResult[T] = atLeast(times).atMost(times)
  def exactly(n: Int): ContainWithResult[T] = exactly(Times(n))

  def forall = copy(timesMin = None, timesMax = None, checkAll = false)
  def foreach = copy(timesMin = None, timesMax = None)

  private def messages[S <: Traversable[T]](expectable: String, successes: Seq[Result], failures: Seq[Result]) =
    def equalValueCheckMessages(expected: Any) =
      val containsMessage = s"$expectable contains $expected"
      val doesNotContainMessage = s"$expectable does not contain $expected"
      (containsMessage, if successes.isEmpty then doesNotContainMessage else containsMessage)

    check match
      case BeEqualTypedValueCheck(expected) => equalValueCheckMessages(expected)
      case BeEqualValueCheck(expected)      => equalValueCheckMessages(expected)
      case _                                => genericMessages(expectable, successes, failures)

  private def genericMessages(
      expectable: String,
      successes: scala.collection.Seq[Result],
      failures: scala.collection.Seq[Result]
  ) =
    def elementsAre(results: scala.collection.Seq[Result], success: Boolean) =
      if results.isEmpty then (if success then "There are no matches" else "There are no failures")
      else if results.size <= 1 then s"There is ${results.size} ${if success then "success" else "failure"}"
      else s"There are ${results.size} ${if success then "successes" else "failures"}"

    def messages(results: scala.collection.Seq[Result]) =
      if results.isEmpty then "\n"
      else results.map(_.message).mkString("\n", "\n", "\n")

    (
      elementsAre(successes, success = true) + messages(successes),
      elementsAre(failures, success = false) + messages(failures)
    )

  override def not =
    copy(negate = !negate)

case class ContainWithResultSeq[T](
    checks: Seq[ValueCheck[T]],
    containsAtLeast: Boolean = true,
    containsAtMost: Boolean = false,
    eachCheck: Boolean = false,
    checkOrder: Boolean = false,
    negate: Boolean = false
) extends Matcher[Traversable[T]]:

  def apply[S <: Traversable[T]](t: Expectable[S]) =
    val seq = t.value.toSeq

    // results for each element, either checked in order or
    // trying to find the best matching from the list of checks
    // return the matched values + the list of checks which were not performed
    val (results, remainingChecks): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) =
      if checkOrder then checkValuesInOrder(seq, checks, eachCheck)
      else checkValues(seq, checks, eachCheck)

    val (successes, failures) = results.partition(rs => rs._2.nonEmpty && rs._2.forall(_.isSuccess))
    val missingValues = remainingChecks.collect(expectedValue).flatten
    val failedValues = failures.map(_._1)

    def makeResult(constraint: String, success: Boolean): Result =
      val equalChecks = checks.forall(isEqualCheck)
      val order = if checkOrder then " in order" else ""
      if equalChecks then
        val missingValues = remainingChecks.collect(expectedValue).flatten
        val addedValues = seq.diff(successes.map(_._1))
        val failedValues = failures.map(_._1).reverse
        if failedValues.isEmpty then
          if missingValues.isEmpty then
            if addedValues.isEmpty then Result.result(success, s"${t.description} contains all expected values")
            else Result.result(success, s"${t.description} contains ${addedValues.mkString(",")}")
          else if eachCheck && seq.exists(missingValues.contains) then
            Result.result(
              success,
              s"${t.description} is missing the ${"value".plural(missingValues)}: ${missingValues.mkString(", ")}"
            )
          else if checkOrder then
            val verb = if missingValues.size > 1 then "are" else "is"
            Result.result(
              success,
              s"the ${"value".plural(missingValues)} ${missingValues.mkString(", ")} $verb missing or not in order\nGot: " + seq
            )
          else Result.result(success, s"${t.description} does not contain ${missingValues.mkString(", ")}")
        else if missingValues.isEmpty then
          Result.result(success, s"${t.description} contains ${failedValues.mkString(", ")}")
        else if (eachCheck && seq.exists(missingValues.contains)) || !checkOrder then
          Result.result(
            success,
            s"${t.description} does not contain ${missingValues
                .mkString(", ")}\nFailures:\n  ${failures.map((v, ms) => s"$v: ${ms.mkString(", ")}").mkString("\n  ")}"
          )
        else
          val verb = if missingValues.size > 1 then "are" else "is"
          Result.result(
            success,
            s"the ${"value".plural(missingValues)} ${missingValues.mkString(", ")} $verb missing or not in order\nGot: " + seq
          )
      else
        val qty = s"$constraint ${checks.size}"
        val values = s"correct ${"value".plural(checks.size)}$order"

        Result.result(
          success,
          s"${t.description} does not contain $qty $values" +
            (if failures.isEmpty then ""
             else
               failures
                 .map { case (value, rs) => "- " + value + "\n" + rs.map(" * " + _).mkString("\n") }
                 .mkString("\n", "\n", "\n")
            )
        )

    val r =
      (containsAtLeast, containsAtMost) match
        case (true, false) =>
          makeResult(
            "at least",
            missingValues.isEmpty &&
              !eachCheck && results.map(_._2).flatten.count(_.isSuccess) >= checks.size ||
              eachCheck && (seq.isEmpty && checks.size == 0 ||
                checks.nonEmpty && results.map(_._2).flatten.count(_.isSuccess) >= checks.size ||
                checks.isEmpty && successes.isEmpty)
          )

        case (false, true) =>
          makeResult(
            "at most",
            failedValues.isEmpty && (!eachCheck || successes.size <= checks.size && successes.size >= seq.size)
          )

        case (true, true) =>
          makeResult("exactly", successes.size == checks.size && checks.size == seq.size)

        case (false, false) =>
          makeResult("", successes.size <= checks.size && checks.size <= seq.size)

    if negate then Result.result(!r.isSuccess, "Expectation failed:\n" + r.message)
    else r

  /** take each value in order and try to apply the first check of the list of checks if that check is successful,
    * remove the value from the list of values to check and remove the check as well otherwise try the next check for
    * the *next* value
    *
    * @return
    *   (the list of all the results for each tested value, the list of remaining checks if any)
    */
  @tailrec
  private def checkValuesInOrder(
      values: Seq[T],
      checks: Seq[ValueCheck[T]],
      eachCheck: Boolean,
      results: Seq[(T, Seq[Result])] = Seq()
  ): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) =
    (values, checks) match
      case (v +: vs, c +: cs) if eachCheck =>
        val r = c.check(v)
        if r.isSuccess then checkValuesInOrder(vs, cs, eachCheck, results :+ (v -> Seq(r)))
        else checkValuesInOrder(vs, checks, eachCheck, results :+ (v -> Seq(r)))

      case (v +: vs, cs @ (_ +: _)) =>
        val checked = cs.map(c => (c, c.check(v)))
        val (successes, failures) = checked.span(_._2.isSuccess)
        val remainingChecks = checked.drop(successes.size).map(_._1)

        if successes.nonEmpty then
          checkValuesInOrder(vs, remainingChecks, eachCheck, results :+ (v -> successes.map(_._2)))
        else checkValuesInOrder(vs, remainingChecks, eachCheck, results :+ (v -> failures.map(_._2)))

      case (v +: vs, nil) => (results :+ (v -> Seq(Failure("is unexpected", v.notNull))), checks)
      case _              => (results, checks)

  /** take each value in order and try to apply the checks of the list of checks keep the result corresponding to the
    * first successful check and return the list of remaining checks to be used on the other values
    *
    * @return
    *   (the list of each result for each tested value, the list of remaining checks if any)
    */
//  @tailrec
  private def checkValues(
      values: Seq[T],
      checks: Seq[ValueCheck[T]],
      eachCheck: Boolean,
      results: Seq[(T, Seq[Result])] = Seq()
  ): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) =
    val (results: Seq[(T, ValueCheck[T], Result)], unchecked) =
      BestMatching.findBestMatch(values, checks, (t: T, check: ValueCheck[T]) => check.check(t), eachCheck)

    val resultsAsSeq = results.map { case (t, v, r) => (t, Seq(r)) }
    (resultsAsSeq, unchecked)

  /** @return
    *   (the result of evaluating value with uncheckedChecks, unchecked and failed checks)
    */
  @tailrec
  private def checkValue(
      value: T,
      uncheckedChecks: Seq[ValueCheck[T]],
      checkedChecks: Seq[ValueCheck[T]],
      eachCheck: Boolean,
      results: Seq[Result]
  ): (Seq[Result], Seq[ValueCheck[T]]) =
    uncheckedChecks match
      case currentCheck +: remainingUncheckedChecks if eachCheck =>
        val result = currentCheck.check(value)
        if result.isSuccess then (Seq(result), checkedChecks ++ remainingUncheckedChecks)
        else checkValue(value, remainingUncheckedChecks, checkedChecks :+ currentCheck, eachCheck, results :+ result)

      case allChecks @ (_ +: _) =>
        val (successes, failures) = allChecks.map(c => (c, c.check(value))).partition(_._2.isSuccess)
        if successes.nonEmpty then (successes.map(_._2).take(1), checkedChecks ++ failures.map(_._1))
        else (Nil, checkedChecks ++ allChecks)

      case _ => (results, checkedChecks)

  private def isEqualCheck = (c: ValueCheck[T]) =>
    c match
      case _: BeEqualTypedValueCheck[T] => true
      case _: BeEqualValueCheck[T]      => true
      case _                            => false

  private def expectedValue: PartialFunction[ValueCheck[T], Option[Any]] =
    case BeEqualTypedValueCheck(e) => Some(e)
    case BeEqualValueCheck(e)      => Some(e)
    case _                         => None

  def atLeast = copy(containsAtLeast = true, containsAtMost = false, eachCheck = true)
  def atMost = copy(containsAtLeast = false, containsAtMost = true, eachCheck = true)
  def exactly = copy(containsAtLeast = true, containsAtMost = true, eachCheck = true)
  def inOrder = copy(checkOrder = true)

  def onDistinctValues: ContainWithResultSeq[T] = onDistinctValues(true)
  def onDistinctValues(distinct: Boolean): ContainWithResultSeq[T] = copy(eachCheck = distinct)

  override def not = copy(negate = !negate)
