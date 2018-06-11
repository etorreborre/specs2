package org.specs2
package matcher

import control._
import data.Sized
import text.Regexes._
import text.Plural._
import text.NotNullStrings._
import collection.Seqx._

import scala.collection.Traversable
import execute._
import control.Times
import execute.Failure

import scala.annotation.tailrec
import ValueChecks._
import org.specs2.matcher.describe.Diffable
/**
 * Matchers for traversables
 */
trait TraversableMatchers extends TraversableBaseMatchers
  with TraversableBeHaveMatchers
  with LazyParameters
  with TraversableBaseMatchersLowImplicits
  with ImplicitParameters
  with NumberOfTimes

object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchers { outer =>
  
  trait TraversableMatcher[T] extends Matcher[Traversable[T]]

  /**
   * ELEMENTS MATCHERS
   */
  def contain[T](check: ValueCheck[T]): ContainWithResult[T] = new ContainWithResult(check)

  /**
   * COLLECTION MATCHERS
   */
  def contain[T](cm: ContainWithResultSeq[T]): ContainWithResultSeq[T] = cm

  def allOf[T](checks: ValueCheck[T]*)  : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast.onDistinctValues(false)
  def eachOf[T](checks: ValueCheck[T]*)  : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def exactly[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).exactly
  def atLeast[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def atMost[T](checks: ValueCheck[T]*) : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atMost

  /** match if a traversable contains all the elements of seq (and maybe more) */
  def containAllOf[T : Diffable](seq: Seq[T]) = contain(atLeast(seq.map(v => valueIsTypedValueCheck(v)):_*))
  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]) = contain(new BeOneOf(seq))
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String) = containPattern[T](t.regexPart)
  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String) = ContainWithResult(matcherIsValueCheck(new BeMatching(t))) ^^ ((ts: Traversable[T]) => ts.toSeq.map(_.toString))

  /** does a containAll comparison in both ways */
  def containTheSameElementsAs[T](seq: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)): Matcher[Traversable[T]] = new Matcher[Traversable[T]] {

    def apply[S <: Traversable[T]](t: Expectable[S]) = {
      val missing = seq.difference(t.value.toSeq, equality)
      val added   = t.value.toSeq.difference(seq, equality)
      def message(diffs: Seq[_], msg: String) =
        if (diffs.isEmpty) "" else diffs.mkString("\n  "+msg+": ", ", ", "")

      result(missing.isEmpty && added.isEmpty,
             t.value + "\n  contains the same elements as\n"+ seq,
             t.value + message(missing, "is missing") + message(added, "must not contain"),
             t)
    }
  }

  /**
   * SIZE MATCHERS
   */

  /** match if there is a way to size T */
  def haveSize[T : Sized](n: Int) = new SizedMatcher[T](n, "size")
  /** alias for haveSize */
  def size[T : Sized](n: Int) = haveSize[T](n)
  /** alias for haveSize */
  def haveLength[T : Sized](n: Int) = new SizedMatcher[T](n, "length")
  /** alias for haveSize */
  def length[T : Sized](n: Int) = haveLength[T](n)

  /** match if there is a way to size T */
  def haveSize[T : Sized](check: ValueCheck[Int]) = new SizedCheckedMatcher[T](check, "size")
  /** alias for haveSize */
  def size[T : Sized](check: ValueCheck[Int]) = haveSize[T](check)
  /** alias for haveSize */
  def haveLength[T : Sized](check: ValueCheck[Int]) = new SizedCheckedMatcher[T](check, "length")
  /** alias for haveSize */
  def length[T : Sized](check: ValueCheck[Int]) = haveLength[T](check)

  /** @return a matcher checking if the elements are ordered */
  def beSorted[T : Ordering] = new OrderingMatcher[T]
  /** alias for beSorted */
  def sorted[T : Ordering] = beSorted[T]

  /**
   * Additional contain methods using to avoid automatic tuple conversions
   */

  // 2 to 5
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5))
  // 6 to 10
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10))
  // 11 to 15
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15))
  // 16 to 20
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T], t18: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T], t18: ValueCheck[T], t19: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T], t18: ValueCheck[T], t19: ValueCheck[T], t20: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20))
  // 21 to 22
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T], t18: ValueCheck[T], t19: ValueCheck[T], t20: ValueCheck[T], t21: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t21, t21))
  def contain[T](t1: ValueCheck[T], t2: ValueCheck[T], t3: ValueCheck[T], t4: ValueCheck[T], t5: ValueCheck[T], t6: ValueCheck[T], t7: ValueCheck[T], t8: ValueCheck[T], t9: ValueCheck[T], t10: ValueCheck[T], t11: ValueCheck[T], t12: ValueCheck[T], t13: ValueCheck[T], t14: ValueCheck[T], t15: ValueCheck[T], t16: ValueCheck[T], t17: ValueCheck[T], t18: ValueCheck[T], t19: ValueCheck[T], t20: ValueCheck[T], t21: ValueCheck[T], t22: ValueCheck[T]): ContainWithResultSeq[T] = contain(allOf(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t21, t22))
}

private[specs2]
trait TraversableBaseMatchersLowImplicits extends ValueChecksLowImplicits { this: TraversableBaseMatchers =>
  implicit def checkableSeqIsContainCheckSeq[T](seq: Seq[T])(implicit to: T => ValueCheck[T]): Seq[ValueCheck[T]] =
    seq.map(to)

  implicit def matcherSeqIsContainCheckSeq[T](seq: Seq[Matcher[T]]): Seq[ValueCheck[T]] =
    seq.map(matcherIsValueCheck[T])

}

private[specs2]
trait TraversableBeHaveMatchers extends LazyParameters with BeHaveMatchers  { outer: TraversableMatchers =>

  implicit def traversable[T](s: MatchResult[Traversable[T]]): TraversableBeHaveMatchers[T] =
    new TraversableBeHaveMatchers(s)

  class TraversableBeHaveMatchers[T](s: MatchResult[Traversable[T]]) {
    def contain(check: ValueCheck[T]) = s(outer.contain(check))
    def containPattern(t: =>String) = s(outer.containPattern(t))
    def containMatch(t: =>String) = containPattern(t.regexPart)
  }

  implicit def sized[T : Sized](s: MatchResult[T]): HasSize[T] =
    new HasSize(s)

  class HasSize[T : Sized](s: MatchResult[T]) {
    def size(n: Int) : MatchResult[T] = s(outer.haveSize[T](n))
    def length(n: Int) : MatchResult[T] = size(n)
    def size(check: ValueCheck[Int]) : MatchResult[T] = s(outer.haveSize[T](check))
    def length(check: ValueCheck[Int]) : MatchResult[T] = size(check)
  }

  implicit def orderedSeqMatchResult[T : Ordering](result: MatchResult[Seq[T]]): OrderedSeqMatchResult[T] =
    new OrderedSeqMatchResult(result)

  class OrderedSeqMatchResult[T : Ordering](result: MatchResult[Seq[T]]) {
    def sorted = result(outer.beSorted[T])
    def beSorted = result(outer.beSorted[T])
  }

}

class SizedMatcher[T : Sized](n: Int, sizeWord: String) extends Matcher[T] {
  def apply[S <: T](traversable: Expectable[S]) = {
    val s = implicitly[Sized[T]]
    val valueSize = s.size(traversable.value)
    result(valueSize == n,
           traversable.description + " has "+sizeWord+" "+ n,
           s"'${traversable.description}' doesn't have $sizeWord $n but $sizeWord $valueSize", traversable)
  }
}

class SizedCheckedMatcher[T : Sized](check: ValueCheck[Int], sizeWord: String) extends Matcher[T] {
  def apply[S <: T](traversable: Expectable[S]) = {
    val s = implicitly[Sized[T]]
    val valueSize = s.size(traversable.value)
    val checked = check.check(valueSize)
    result(checked.isSuccess,
      traversable.description + " has the right "+sizeWord+": "+ checked.message,
      traversable.description + " doesn't have the right "+sizeWord+": "+ checked.message, traversable)
  }
}

class OrderingMatcher[T : Ordering] extends Matcher[Seq[T]] {
  def apply[S <: Seq[T]](traversable: Expectable[S]) = {
    result(traversable.value == traversable.value.sorted,
      traversable.description + " is sorted",
      traversable.description + " is not sorted", traversable)
  }
}
import control.NumberOfTimes._
import text.Plural._

case class ContainWithResult[T](check: ValueCheck[T], timesMin: Option[Times] = Some(1.times), timesMax: Option[Times] = None, checkAll: Boolean = true) extends Matcher[Traversable[T]] {
  def apply[S <: Traversable[T]](t: Expectable[S]) = {
    val seq = Vector(t.value.seq.toSeq:_*)

    // stop after the first failure if !checkAll
    val (successes, failures) = seq.foldLeft((Seq[Result](), Seq[Result]())) { (res, cur) =>
      val (ss, fs) = res
      if (!checkAll && fs.nonEmpty) res
      else {
        check.check(cur) match {
          case e: Error         => throw new ErrorException(e)
          case r if r.isSuccess => (ss :+ r, fs)
          case r                => (ss, fs :+ r)
        }
      }
    }

    failures.collect { case s: Skipped => MatchSkip(s.message, t) }.headOption.getOrElse {
      val (okMessage, koMessage) = messages(t.description, successes, failures)
      val details: Details = failures.collect { case Failure(_,_,_,d) if d != NoDetails => d }.headOption.getOrElse(NoDetails)

      (timesMin, timesMax) match {
        case (None,             None)             => Matcher.result(successes.size == seq.size,                     okMessage, koMessage, t, details)
        case (Some(Times(min)), None)             => Matcher.result(successes.size >= min,                          okMessage, koMessage, t, details)
        case (None,             Some(Times(max))) => Matcher.result(successes.size <= max,                          okMessage, koMessage, t, details)
        case (Some(Times(min)), Some(Times(max))) => Matcher.result(successes.size >= min && successes.size <= max, okMessage, koMessage, t, details)
      }
    }
  }

  def atLeastOnce                    : ContainWithResult[T] = atLeast(1.times)
  def atLeast(times: Times)          : ContainWithResult[T] = copy(timesMin = Option(times))
  def atLeast(n: Int)                : ContainWithResult[T] = atLeast(Times(n))

  def atMostOnce                     : ContainWithResult[T] = atMost(1.times)
  def atMost(times: Times)           : ContainWithResult[T] = copy(timesMax = Option(times))
  def atMost(n: Int)                 : ContainWithResult[T] = atMost(Times(n))

  def between(min: Times, max: Times): ContainWithResult[T] = atLeast(min).atMost(max)
  def between(min: Int, max: Int)    : ContainWithResult[T] = between(Times(min), Times(max))

  def exactly(times: Times)          : ContainWithResult[T] = atLeast(times).atMost(times)
  def exactly(n: Int)                : ContainWithResult[T] = exactly(Times(n))

  def forall  = copy(timesMin = None, timesMax = None, checkAll = false)
  def foreach = copy(timesMin = None, timesMax = None)

  private
  def messages[S <: Traversable[T]](expectable: String, successes: Seq[Result], failures: Seq[Result]) = check match {
    case BeEqualTypedValueCheck(expected) => (s"$expectable contains $expected", s"$expectable does not contain $expected")
    case BeEqualValueCheck(expected)      => (s"$expectable contains $expected", s"$expectable does not contain $expected")
    case _                                => genericMessages(expectable, successes, failures)
  }

  private
  def genericMessages(expectable: String, successes: Seq[Result], failures: Seq[Result]) = {
    def elementsAre(results: Seq[Result], success: Boolean) =
      if   (results.isEmpty)      s"There are no matches"
      else if (results.size <= 1) s"There is ${results.size} ${if (success) "success" else "failure"}"
      else                        s"There are ${results.size} ${if (success) "successes" else "failures"}"

    def messages(results: Seq[Result]) =
      if (results.isEmpty) ""
      else                 results.map(_.message).mkString("\n", "\n", "\n")

    (elementsAre(successes, success = true) + messages(successes),
     elementsAre(failures, success = false) + messages(failures))
  }

}

case class ContainWithResultSeq[T](checks: Seq[ValueCheck[T]],
                                   containsAtLeast: Boolean = true,
                                   containsAtMost: Boolean = false,
                                   eachCheck: Boolean = false,
                                   checkOrder: Boolean = false,
                                   negate: Boolean = false) extends Matcher[Traversable[T]] {

  def apply[S <: Traversable[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq

    // results for each element, either checked in order or
    // trying to find the best matching from the list of checks
    // return the matched values + the list of checks which were not performed
    val (results, remainingChecks): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) =
      if (checkOrder) checkValuesInOrder(seq, checks, eachCheck)
      else            checkValues(seq, checks, eachCheck)

    val (successes, failures) = results.partition(rs => rs._2.nonEmpty && rs._2.forall(_.isSuccess))
    val missingValues = remainingChecks.collect(expectedValue).flatten
    val failedValues  = failures.map(_._1)

    def makeResult(constraint: String, success: Boolean): MatchResult[S] = {
      val equalChecks = checks.forall(isEqualCheck)
      val order = if (checkOrder) " in order" else ""
      if (equalChecks) {
        val missingValues = remainingChecks.collect(expectedValue).flatten
        val addedValues   = seq.diff(successes.map(_._1))
        val failedValues  = failures.map(_._1)
        if (failedValues.isEmpty)
          if (missingValues.isEmpty){
            if (addedValues.isEmpty)
              Matcher.result(success, s"${t.description} contains all expected values", t)
            else
              Matcher.result(success, s"${t.description} contains ${addedValues.mkString(",")}", t)
          }
          else {
            if (eachCheck && seq.exists(missingValues.contains))
              Matcher.result(success, s"${t.description} is missing the ${"value".plural(missingValues)}: ${missingValues.mkString(", ")}", t)
            else if (checkOrder) {
              val verb = if (missingValues.size > 1) "are" else "is"
              Matcher.result(success, s"the ${"value".plural(missingValues)} ${missingValues.mkString(", ")} $verb not in order", t)
            }
            else
              Matcher.result(success, s"${t.description} does not contain ${missingValues.mkString(", ")}", t)
          }
        else
        if (missingValues.isEmpty) {
          Matcher.result(success, s"${t.description} contains ${failedValues.mkString(", ")}", t)
        }
        else {
          if (eachCheck && seq.exists(missingValues.contains))
            Matcher.result(success, s"${t.description} is missing the ${"value".plural(missingValues)}: ${missingValues.mkString(", ")} but contains ${failedValues.mkString(", ")}", t)
          else if (checkOrder) {
            val verb = if (missingValues.size > 1) "are" else "is"
            Matcher.result(success, s"the ${"value".plural(missingValues)} ${missingValues.mkString(", ")} $verb not in order", t)
          }
          else
            Matcher.result(success, s"${t.description} does not contain ${missingValues.mkString(", ")} but contains ${failedValues.mkString(", ")}", t)
        }
      } else {
        val qty     = s"$constraint ${checks.size}"
        val values  = s"correct ${"value".plural(checks.size)}$order"

        Matcher.result(success,
          s"${t.description} does not contain $qty $values" +
            (if (failures.isEmpty) ""
            else failures.map { case (value, rs) => "- "+value+"\n"+rs.map(" * "+_).mkString("\n") }.mkString("\n", "\n", "\n")), t)
      }
    }

    val r =
      (containsAtLeast, containsAtMost) match {
        case (true,  false) =>
          makeResult("at least",
            missingValues.isEmpty &&
              !eachCheck && results.map(_._2).flatten.count(_.isSuccess) >= checks.size ||
               eachCheck && (
                  seq.isEmpty && checks.size == 0 ||
                  checks.nonEmpty && results.map(_._2).flatten.count(_.isSuccess) >= checks.size ||
                  checks.isEmpty && successes.isEmpty))

        case (false, true)  =>
          makeResult("at most", failedValues.isEmpty && (!eachCheck || successes.size <= checks.size && successes.size >= seq.size))

        case (true,  true)  =>
          makeResult("exactly", successes.size == checks.size && checks.size == seq.size)

        case (false, false) =>
          makeResult("", successes.size <= checks.size && checks.size <= seq.size)
      }

    if (negate) Matcher.result(!r.isSuccess, r.message, r.message, t)
    else r
  }



  /**
   * take each value in order and try to apply the first check of the list of checks
   * if that check is successful, remove the value from the list of values to check and remove the check as well
   * otherwise try the next check for the *next* value
   *
   * @return (the list of all the results for each tested value, the list of remaining checks if any)
   */
  @tailrec
  private def checkValuesInOrder(values: Seq[T], checks: Seq[ValueCheck[T]], eachCheck: Boolean, results: Seq[(T, Seq[Result])] = Seq()): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) = {
    (values, checks) match {
      case (v +: vs, c +: cs) if eachCheck =>
        val r = c.check(v)
        if (r.isSuccess) checkValuesInOrder(vs, cs, eachCheck, results :+ (v -> Seq(r)))
        else             checkValuesInOrder(vs, checks, eachCheck, results :+ (v -> Seq(r)))

      case (v +: vs, cs @ (_ +: _)) =>
        val checked = cs.map(c => (c, c.check(v)))
        val (successes, failures) = checked.span(_._2.isSuccess)
        val remainingChecks = checked.drop(successes.size).map(_._1)

        if (successes.nonEmpty) checkValuesInOrder(vs, remainingChecks, eachCheck, results :+ (v -> successes.map(_._2)))
        else                    checkValuesInOrder(vs, remainingChecks, eachCheck, results :+ (v -> failures.map(_._2)))

      case (v +: vs, nil) => (results :+ (v -> Seq(Failure("is unexpected", v.notNull))), checks)
      case _              => (results, checks)
    }
  }

  /**
   * take each value in order and try to apply the checks of the list of checks
   * keep the result corresponding to the first successful check and return the list of remaining checks to be used on the other values
   *
   * @return (the list of each result for each tested value, the list of remaining checks if any)
   */
//  @tailrec
  private def checkValues(values: Seq[T], checks: Seq[ValueCheck[T]], eachCheck: Boolean, results: Seq[(T, Seq[Result])] = Seq()): (Seq[(T, Seq[Result])], Seq[ValueCheck[T]]) = {
    val (results: Seq[(T, ValueCheck[T], Result)], unchecked) =
      BestMatching.findBestMatch(values, checks, (t: T, check: ValueCheck[T]) => check.check(t), eachCheck)

    val resultsAsSeq = results.map { case (t, v, r) => (t, Seq(r)) }
    (resultsAsSeq, unchecked)
  }


  /**
   * @return (the result of evaluating value with uncheckedChecks, unchecked and failed checks)
   */
  @tailrec
  private def checkValue(value: T, uncheckedChecks: Seq[ValueCheck[T]], checkedChecks: Seq[ValueCheck[T]], eachCheck: Boolean, results: Seq[Result]) : (Seq[Result], Seq[ValueCheck[T]]) = {
    uncheckedChecks match {
      case currentCheck +: remainingUncheckedChecks if eachCheck =>
        val result = currentCheck.check(value)
        if (result.isSuccess) (Seq(result), checkedChecks ++ remainingUncheckedChecks)
        else                   checkValue(value, remainingUncheckedChecks, checkedChecks :+ currentCheck, eachCheck, results :+ result)

      case allChecks @ (_ +: _) =>
        val (successes, failures) = allChecks.map(c => (c, c.check(value))).partition(_._2.isSuccess)
        if (successes.nonEmpty) (successes.map(_._2).take(1), checkedChecks ++ failures.map(_._1))
        else                    (Nil, checkedChecks ++ allChecks)

      case _ => (results, checkedChecks)
    }
  }

  private def isEqualCheck = (c: ValueCheck[T]) => c match {
    case _:BeEqualTypedValueCheck[T] => true
    case _:BeEqualValueCheck[T]      => true
    case _                           => false
  }

  private def expectedValue: PartialFunction[ValueCheck[T], Option[Any]] = {
    case BeEqualTypedValueCheck(e) => Some(e)
    case BeEqualValueCheck(e)      => Some(e)
    case _                         => None
  }

  def atLeast = copy(containsAtLeast = true, containsAtMost = false, eachCheck = true)
  def atMost  = copy(containsAtLeast = false, containsAtMost = true, eachCheck = true)
  def exactly = copy(containsAtLeast = true, containsAtMost = true, eachCheck = true)
  def inOrder = copy(checkOrder = true)

  def onDistinctValues: ContainWithResultSeq[T]                    = onDistinctValues(true)
  def onDistinctValues(distinct: Boolean): ContainWithResultSeq[T] = copy(eachCheck = distinct)

  override def not = copy(negate = !negate)
}

