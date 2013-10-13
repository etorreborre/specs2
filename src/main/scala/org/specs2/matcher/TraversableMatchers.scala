package org.specs2
package matcher

import control._
import data.Sized
import text.Quote._
import text.Regexes._
import text.Plural._
import text.NotNullStrings._
import collection.Iterablex._
import collection.Seqx._
import MatchersImplicits._
import scala.collection.{GenSeq, GenTraversableOnce, GenTraversable}
import execute._
import control.Times
import ValueChecks._
import execute.Failure
import scala.annotation.tailrec
/**
 * Matchers for traversables
 */
trait TraversableMatchers extends TraversableBaseMatchers with NumberOfTimes with TraversableBeHaveMatchers with LazyParameters
object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchers extends ValueChecks with TraversableBaseMatchersLowImplicits with ImplicitParameters { outer =>
  
  trait TraversableMatcher[T] extends Matcher[GenTraversableOnce[T]]

  /**
   * ELEMENTS MATCHERS
   */
  def contain[T](check: ValueCheck[T]): ContainWithResult[T] = new ContainWithResult(check)

  /**
   * COLLECTION MATCHERS
   */
  def contain[T](cm: ContainWithResultSeq[T]): ContainWithResultSeq[T] = cm

  def allOf[T](checks: ValueCheck[T]*)  : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def exactly[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).exactly
  def atLeast[T](checks: ValueCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def atMost[T](checks: ValueCheck[T]*) : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atMost

  /** match if a traversable contains all the elements of seq (and maybe more) */
  def containAllOf[T](seq: Seq[T]) = contain(atLeast(seq.map(v => valueIsTypedContainCheck(v)):_*))
  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]) = contain(new BeOneOf(seq))
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String) = containPattern[T](t.regexPart)
  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String) = ContainWithResult(matcherIsContainCheck(new BeMatching(t))) ^^ ((ts: GenTraversableOnce[T]) => ts.toSeq.map(_.toString).to[GenTraversableOnce])

  /** does a containAll comparison in both ways */
  def containTheSameElementsAs[T](seq: Seq[T], equality: (T, T) => Boolean = (_:T) == (_:T)): Matcher[Traversable[T]] = new Matcher[Traversable[T]] {

    def apply[S <: Traversable[T]](t: Expectable[S]) = {
      val missing = seq.toSeq.difference(t.value.toSeq, equality)
      val added   = t.value.toSeq.difference(seq.toSeq, equality)
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

  /** @return a matcher checking if the elements are ordered */
  def beSorted[T : Ordering] = new OrderingMatcher[T]
  /** alias for beSorted */
  def sorted[T : Ordering] = beSorted[T]

  /** any scala collection has a size */
  implicit def scalaTraversableIsSized[I <: GenTraversableOnce[_]]: Sized[I] = new Sized[I] {
    def size(t: I) = t.size
  }
  /** any scala array has a size */
  implicit def scalaArrayIsSized[T]: Sized[Array[T]] = new Sized[Array[T]] {
    def size(t: Array[T]) = t.length
  }
  /** any java collection has a size */
  implicit def javaCollectionIsSized[T <: java.util.Collection[_]]: Sized[T] = new Sized[T] {
    def size(t: T) = t.size()
  }
  /** a regular string has a size, without having to be converted to an Traversable */
  implicit def stringIsSized: Sized[String] = new Sized[String] {
    def size(t: String) = t.size
  }

}

private[specs2]
trait TraversableBaseMatchersLowImplicits extends ValueChecksLowImplicits { this: TraversableBaseMatchers =>
  implicit def checkableSeqIsContainCheckSeq[T](seq: Seq[T])(implicit to: T => ValueCheck[T]): Seq[ValueCheck[T]] =
    seq.map(to)

  implicit def matcherSeqIsContainCheckSeq[T](seq: Seq[Matcher[T]]): Seq[ValueCheck[T]] =
    seq.map(matcherIsContainCheck[T])

  /** this allows the contain(string) matcher for StringMatchers to be used with a Traversable */
  implicit def stringMatcherIsTraversableMatcher(m: Matcher[String]): Matcher[GenTraversableOnce[String]] =
    contain(matcherIsContainCheck(m))

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
trait TraversableBeHaveMatchers extends LazyParameters { outer: TraversableMatchers =>

  implicit def traversable[T](s: MatchResult[Traversable[T]]) = new TraversableBeHaveMatchers(s)
  class TraversableBeHaveMatchers[T](s: MatchResult[Traversable[T]]) {
    def contain(check: ValueCheck[T]) = s(outer.contain(check))
    def containPattern(t: =>String) = s(outer.containPattern(t))
    def containMatch(t: =>String) = containPattern(t.regexPart)
    /** @deprecated(message="use contain(function) instead", since="2.0") */
    def have(f: T => Boolean) = s(outer.have(f))
    /** @deprecated(message="use contain(like(partialFunction)) instead", since="2.0") */
    def oneElementLike[U](like: PartialFunction[T, MatchResult[U]]) = s(outer.haveOneElementLike(like))
    /** @deprecated(message="use contain(like(partialFunction)).forall instead", since="2.0") */
    def allElementsLike[U](like: PartialFunction[T, MatchResult[U]]) = s(outer.haveAllElementsLike(like))
  }

  implicit def sized[T : Sized](s: MatchResult[T]) = new HasSize(s)
  class HasSize[T : Sized](s: MatchResult[T]) {
    def size(n: Int) : MatchResult[T] = s(outer.haveSize[T](n))
    def length(n: Int) : MatchResult[T] = size(n)
  }

  implicit def orderedSeqMatchResult[T : Ordering](result: MatchResult[Seq[T]]) = new OrderedSeqMatchResult(result)
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
           traversable.description + " have "+sizeWord+" "+ n,
           traversable.description + " doesn't have "+sizeWord+" " + n + " but "+sizeWord+" " + valueSize, traversable)
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

case class ContainWithResult[T](check: ValueCheck[T], timesMin: Option[Times] = Some(1.times), timesMax: Option[Times] = None, checkAll: Boolean = true) extends Matcher[GenTraversableOnce[T]] {
  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = Vector(t.value.seq.toSeq:_*)

    // stop after the first failure if !checkAll
    val (successes, failures) = seq.foldLeft(Seq[Result](), Seq[Result]()) { (res, cur) =>
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

      (timesMin, timesMax) match {
        case (None,             None)             => Matcher.result(successes.size == seq.size,                     okMessage, koMessage, t)
        case (Some(Times(min)), None)             => Matcher.result(successes.size >= min,                          okMessage, koMessage, t)
        case (None,             Some(Times(max))) => Matcher.result(successes.size <= max,                          okMessage, koMessage, t)
        case (Some(Times(min)), Some(Times(max))) => Matcher.result(successes.size >= min && successes.size <= max, okMessage, koMessage, t)
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
  def messages[S <: GenTraversableOnce[T]](expectable: String, successes: Seq[Result], failures: Seq[Result]) = check match {
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

    def messages(results: Seq[Result]) = if (results.isEmpty) "" else results.map(_.message).mkString("\n", "\n", "\n")

    (elementsAre(successes, success = true) + messages(successes),
     elementsAre(failures, success = false) + messages(failures))
  }

}

case class ContainWithResultSeq[T](checks: Seq[ValueCheck[T]],
                                   containsAtLeast: Boolean = true,
                                   containsAtMost: Boolean = false,
                                   checkOrder: Boolean = false) extends Matcher[GenTraversableOnce[T]] {

  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq

    // results for each element, either checked in order or greedily from the list of checks
    // + the list of checks which were not performed
    val (results, remainingChecks): (Seq[Result], Seq[ValueCheck[T]]) =
      if (checkOrder) checkValuesInOrder(seq, checks)
      else            checkValues(seq, checks)

    val (successes, failures) = results.partition(_.isSuccess)
    val koMessage = makeKoMessage(t.description, successes, failures, remainingChecks)
    val okMessage = negateSentence(koMessage)

    (containsAtLeast, containsAtMost) match {
      case (true,  false) => Matcher.result(successes.size >= checks.size && checks.size <= seq.size, okMessage , koMessage, t)
      case (false, true)  => Matcher.result(successes.size <= checks.size && checks.size >= seq.size, okMessage , koMessage, t)
      case (true,  true)  => Matcher.result(successes.size == checks.size && checks.size == seq.size, okMessage , koMessage, t)
      case (false, false) => Matcher.result(successes.size <= checks.size && checks.size <= seq.size, okMessage , koMessage, t)
    }
  }

  /**
   * take each value in order and try to apply the first check of the list of checks
   * if that check is successful, remove the value from the list of values to check and remove the check as well
   * otherwise try the next check for the *next* value
   *
   * @return (the list of all the results for each tested value, the list of remaining checks if any)
   */
  @tailrec
  private def checkValuesInOrder(values: Seq[T], checks: Seq[ValueCheck[T]], results: Seq[Result] = Seq()): (Seq[Result], Seq[ValueCheck[T]]) = {
    (values, checks) match {
      case (v +: vs, c +: cs) => {
        val r = c.check(v)
        if (r.isSuccess) checkValuesInOrder(vs, cs, results :+ r)
        else             checkValuesInOrder(vs, checks, results :+ r)
      }
      case (v +: vs, nil) => (results :+ Failure("there are no more available checks for "+v, v.notNull), checks)
      case _              => (results, checks)
    }
  }

  /**
   * take each value in order and try to apply the checks of the list of checks
   * keep the result corresponding to the first successful check and return the list of remaining checks to be used on the other values
   *
   * @return (the list of each result for each tested value, the list of remaining checks if any)
   */
  @tailrec
  private def checkValues(values: Seq[T], checks: Seq[ValueCheck[T]], results: Seq[Result] = Seq()): (Seq[Result], Seq[ValueCheck[T]]) = {
    values match {
      case currentValue +: remainingValues =>
        val (result, uncheckedChecks) = checkValue(currentValue, checks, Seq(), Failure("there are no more available checks for " + currentValue, currentValue.notNull))
        checkValues(remainingValues, uncheckedChecks, results :+ result)

      case _ => (results, checks)
    }
  }


  /**
   * @return (the result of evaluating value with uncheckedChecks, unchecked and failed checks)
   */
  @tailrec
  private def checkValue(value: T, uncheckedChecks: Seq[ValueCheck[T]], checkedChecks: Seq[ValueCheck[T]], previousResult: Result) : (Result, Seq[ValueCheck[T]]) = {
    uncheckedChecks match {
      case currentCheck +: remainingUncheckedChecks =>
        val result = currentCheck.check(value)
        if (result.isSuccess) (result, checkedChecks ++ remainingUncheckedChecks)
        else                  checkValue(value, remainingUncheckedChecks, checkedChecks :+ currentCheck, result or previousResult)

      case _ => (previousResult, checkedChecks)
    }
  }

  private def makeKoMessage(description: String, successes: Seq[Result], failures: Seq[Result], remainingChecks: Seq[ValueCheck[T]]) = {
    val equalChecks = checks.forall(isEqualCheck)
    if (equalChecks) {
      val order = if (checkOrder) " in order" else ""
      val missingValues = remainingChecks.collect(expectedValue).flatten
      val excessValues  = failures.map(_.expected)
      if (missingValues.isEmpty)     s"$description must not contain ${excessValues.mkString(", ")}$order"
      else if (excessValues.isEmpty) s"$description does not contain ${missingValues.mkString(", ")}$order"
      else                           s"$description does not contain ${missingValues.mkString(", ")} and must not contain ${excessValues.mkString(", ")}$order"
    } else {
      val qty =
        if      (containsAtLeast && containsAtMost) s"exactly ${checks.size}"
        else if (containsAtLeast)                   s"at least ${checks.size}"
        else if (containsAtMost)                    s"at most ${checks.size}"
        else                                        s"${checks.size}"

      val order = if (checkOrder) " in order" else ""
      val values = s"correct ${"value".plural(checks.size)}$order"
      s"$description does not contain $qty $values" +
        (if (failures.isEmpty) "" else failures.mkString("\n", "\n", "\n"))
    }

  }

  private def isEqualCheck = (c: ValueCheck[T]) => c match {
    case _:BeEqualTypedValueCheck[T] => true
    case _:BeEqualValueCheck[T]      => true
    case _                             => false
  }

  private def expectedValue: PartialFunction[ValueCheck[T], Option[Any]] = {
    case BeEqualTypedValueCheck(e) => Some(e)
    case BeEqualValueCheck(e)      => Some(e)
    case _                           => None
  }

  def atLeast = copy(containsAtLeast = true, containsAtMost = false)
  def atMost  = copy(containsAtLeast = false, containsAtMost = true)
  def exactly = copy(containsAtLeast = true, containsAtMost = true)

  def inOrder = copy(checkOrder = true)

  override def not = copy(checks = checks.map(_.negate))
}

