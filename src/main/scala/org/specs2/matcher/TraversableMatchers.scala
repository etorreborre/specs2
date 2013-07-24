package org.specs2
package matcher

import control._
import data.Sized
import text.Quote._
import text.Plural._
import collection.Iterablex._
import collection.Seqx._
import MatchersImplicits._
import scala.collection.{GenSeq, GenTraversableOnce, GenTraversable}
import execute._
import control.Times
import ContainCheck._
import ContainChecks._
import execute.Failure
import scala.annotation.tailrec
/**
 * Matchers for traversables
 */
trait TraversableMatchers extends TraversableBaseMatchers with NumberOfTimes with TraversableBeHaveMatchers with DeprecatedTraversableBaseMatchers with LazyParameters
object TraversableMatchers extends TraversableMatchers

private[specs2]
trait TraversableBaseMatchers extends ContainChecks with TraversableBaseMatchersLowImplicits with ImplicitParameters { outer =>
  
  trait TraversableMatcher[T] extends Matcher[GenTraversableOnce[T]]

  /**
   * ELEMENTS MATCHERS
   */
  def contain[T](check: ContainCheck[T]): ContainWithResult[T] = new ContainWithResult(check)

  /**
   * COLLECTION MATCHERS
   */
  def contain[T](cm: ContainWithResultSeq[T]): ContainWithResultSeq[T] = cm

  def allOf[T](checks: ContainCheck[T]*)  : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def exactly[T](checks: ContainCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).exactly
  def atLeast[T](checks: ContainCheck[T]*): ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atLeast
  def atMost[T](checks: ContainCheck[T]*) : ContainWithResultSeq[T] = new ContainWithResultSeq(checks).atMost

  /** match if a traversable contains all the elements of seq (and maybe more) */
  def containAllOf[T](seq: Seq[T]) = contain(atLeast(seq.map(v => valueIsTypedContainCheck(v)):_*))
  /** match if a traversable contains one of (t1, t2) */
  def containAnyOf[T](seq: Seq[T]) = contain(new BeOneOf(seq))
  /** match if traversable contains (x matches .*+t+.*) */
  def containMatch[T](t: =>String) = containPattern[T](".*"+t+".*")
  /** match if traversable contains (x matches p) */
  def containPattern[T](t: =>String) = ContainWithResult(matcherIsContainCheck(new BeMatching(t))) ^^ ((ts: GenTraversableOnce[T]) => ts.toSeq.map(_.toString).to[GenTraversableOnce])

  /** does a containAll comparison in both ways */
  def containTheSameElementsAs[T](seq: Seq[T]): Matcher[Traversable[T]] = new Matcher[Traversable[T]] {

    def apply[S <: Traversable[T]](t: Expectable[S]) = {
      val missing = (seq.toSeq.diff(t.value.toSeq))
      val added   = (t.value.toSeq.diff(seq.toSeq))
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
trait TraversableBaseMatchersLowImplicits { this: TraversableBaseMatchers =>
  implicit def checkableSeqIsContainCheckSeq[T](seq: Seq[T])(implicit to: T => ContainCheck[T]): Seq[ContainCheck[T]] =
    seq.map(to)

  implicit def matcherSeqIsContainCheckSeq[T](seq: Seq[Matcher[T]]): Seq[ContainCheck[T]] =
    seq.map(matcherIsContainCheck[T])

  /** this allows the contain(string) matcher for StringMatchers to be used with a Traversable */
  implicit def stringMatcherIsTraversableMatcher(m: Matcher[String]): Matcher[GenTraversableOnce[String]] =
    contain(matcherIsContainCheck(m))

  implicit def valueIsTypedContainCheck[T](expected: T): BeEqualTypedContainCheck[T] = new BeEqualTypedContainCheck[T](expected)

}

private[specs2]
trait TraversableBeHaveMatchers extends LazyParameters { outer: TraversableMatchers =>

  implicit def traversable[T](s: MatchResult[Traversable[T]]) = new TraversableBeHaveMatchers(s)
  class TraversableBeHaveMatchers[T](s: MatchResult[Traversable[T]]) {
    def contain(check: ContainCheck[T]) = s(outer.contain(check))
    def containPattern(t: =>String) = s(outer.containPattern(t))
    def containMatch(t: =>String) = containPattern(".*"+t+".*")
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

trait ContainCheck[T] { outer =>
  def check:    T => Result
  def checkNot: T => Result

  def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]): (String, String)

  def negate = new ContainCheck[T] {
    def check: T => Result = outer.checkNot
    def checkNot: T => Result = outer.check
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) = outer.messages(expectable, successes, failures)
  }
}

trait ContainChecks {
  implicit def matcherIsContainCheck[T](m: Matcher[T]): ContainCheck[T] = new ContainCheck[T] {
    def check    = (t: T) => AsResult(m(Expectable(t)))
    def checkNot = (t: T) => AsResult(m.not(Expectable(t)))
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
      ContainCheck.genericMessages(expectable, successes, failures)
  }

  implicit def functionIsContainCheck[T, R : AsResult](f: T => R): ContainCheck[T] = new ContainCheck[T] {
    def check    = (t: T) => AsResult(f(t))
    def checkNot = (t: T) => Results.negate(AsResult(f(t)))
    def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
      ContainCheck.genericMessages(expectable, successes, failures)
  }

  implicit def downcastBeEqualTypedContainCheck[T, S >: T](check: BeEqualTypedContainCheck[T]): ContainCheck[S] = check.downcast[S]
}
object ContainChecks extends ContainChecks

case class BeEqualTypedContainCheck[T](expected: T) extends ContainCheck[T] {
  private lazy val matcher = new BeTypedEqualTo(expected)
  def check    = (t: T) => AsResult(matcher(Expectable(t)))
  def checkNot = (t: T) => AsResult(matcher.not(Expectable(t)))
  def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
    (s"$expectable contains $expected", s"$expectable does not contain $expected")

  def downcast[S] = new BeEqualContainCheck[S](expected)
}

case class BeEqualContainCheck[T](expected: Any) extends ContainCheck[T] {
  private lazy val matcher = new BeEqualTo(expected)
  def check    = (t: T) => AsResult(matcher(Expectable(t)))
  def checkNot = (t: T) => AsResult(matcher.not(Expectable(t)))
  def messages(expectable: String, successes: Seq[Result], failures: Seq[Result]) =
    (s"$expectable contains $expected", s"$expectable does not contain $expected")
}

object ContainCheck {
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

case class ContainWithResult[T](check: ContainCheck[T], timesMin: Option[Times] = Some(1.times), timesMax: Option[Times] = None, checkAll: Boolean = true) extends Matcher[GenTraversableOnce[T]] {
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
      val (okMessage, koMessage) = check.messages(t.description, successes, failures)

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
}

case class ContainWithResultSeq[T](checks: Seq[ContainCheck[T]],
                                   containsAtLeast: Boolean = true,
                                   containsAtMost: Boolean = false,
                                   checkOrder: Boolean = false) extends Matcher[GenTraversableOnce[T]] {

  def apply[S <: GenTraversableOnce[T]](t: Expectable[S]) = {
    val seq = t.value.seq.toSeq

    // results for each element, either checked in order or greedily from the list of checks
    val results: Seq[Result] =
      if (checkOrder) {
        @tailrec
        def checkResults(values: Seq[T], checks: Seq[ContainCheck[T]], results: Seq[Result] = Seq()): Seq[Result] = {
          (values, checks) match {
            case (v +: vs, c +: cs) => {
              val r = c.check(v)
              if (r.isSuccess) checkResults(vs, cs, results :+ r)
              else             checkResults(vs, checks, results :+ r)
            }
            case (v +: vs, nil) => results :+ Failure("there are no more available checks for "+v)
            case _              => results
          }
        }
        checkResults(seq, checks)
      } else {
        seq.foldLeft((Seq[Result](), checks)) { (res, cur) =>
          val (results, remainingChecks) = res
          val checksAgainstCurrentElement = remainingChecks.map(c => (c, c.check(cur)))
          (results :+ checksAgainstCurrentElement.map(_._2).foldLeft(Failure("there are no more available checks for "+cur): Result) { (res, cur) => cur or res },
            checksAgainstCurrentElement.removeFirst(_._2.isSuccess).map(_._1))
        }._1
      }

    lazy val (successes, failures) = results.partition(_.isSuccess)

    val qty =
      if      (containsAtLeast && containsAtMost) s"exactly ${checks.size}"
      else if (containsAtLeast)                   s"at least ${checks.size}"
      else if (containsAtMost)                    s"at most ${checks.size}"
      else                                        s"${checks.size}"

    val order = if (checkOrder) " in order" else ""
    val values = s"correct ${"value".plural(checks.size)}$order"
    val okMessage = s"${t.description} has $qty $values"
    val koMessage = negateSentence(okMessage) + (if (failures.isEmpty) "" else failures.mkString("\n", "\n", "\n"))

    (containsAtLeast, containsAtMost) match {
      case (true,  false) => Matcher.result(successes.size >= checks.size && checks.size <= seq.size, okMessage , koMessage, t)
      case (false, true)  => Matcher.result(successes.size <= checks.size && checks.size >= seq.size, okMessage , koMessage, t)
      case _              => Matcher.result(!seq.isEmpty && successes.size == seq.size,               okMessage,  koMessage, t)
    }
  }

  def atLeast = copy(containsAtLeast = true, containsAtMost = false)
  def atMost  = copy(containsAtLeast = false, containsAtMost = true)
  def exactly = copy(containsAtLeast = true, containsAtMost = true)

  def inOrder = copy(checkOrder = true)

  override def not = copy(checks = checks.map(_.negate))
}

