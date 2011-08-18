package org.specs2
package specification

import collection.Iterablex._
import main.Arguments
import execute._
import org.specs2.internal.scalaz.{ Scalaz, Monoid, Reducer }
import Scalaz._
import org.specs2.execute.StandardResults
import text._
import Plural._
import xml.Nodex._
import time._
import specification._
import control.Exceptions._
import org.specs2.control.Exceptions._
import org.specs2.time.SimpleTimer
import scala.xml._

/**
* The Stats class store results for the number of:
* * successes
* * expectations
* * failures
* * errors
* * pending
* * skipped
*
*  for each example
*
*/
case class Stats(examples:     Int = 0,
                 successes:    Int = 0,
                 expectations: Int = 0,
                 failures:     Int = 0,
                 errors:       Int = 0,
                 pending:      Int = 0,
                 skipped:      Int = 0,
                 trend:        Option[Stats] = None,
                 timer:        SimpleTimer = new SimpleTimer) {

  /** @return true if there are errors or failures */
  def hasFailuresOrErrors = failures + errors > 0
  /** @return true if there are expectations */
  def hasExpectations = expectations > 0
  /** @return an equivalent result for display */
  def result =
    if (failures + errors == 0)
      if (successes > 0 || skipped + pending == 0) StandardResults.success
      else if (pending > skipped)                  StandardResults.pending
      else                                         StandardResults.skipped
    else if (errors > 0)           StandardResults.anError
    else                           StandardResults.failure

  /** @return true if there are no issues at all */
  def isSuccess = result.isSuccess
  /** @return true if there are failures or errors */
  def hasIssues = result.isFailure || result.isError
  /** @return the execution time */
  def time = timer.time
  /**
   * @return the xml representation of the statistics. Omit the attributes with 0 as a value for conciseness
   */
  def toXml: Elem = {
    val stats = <stats>{trend.map(t => <trend>{t.toXml}</trend>).getOrElse(NodeSeq.Empty)}</stats>
    val attributes = Map(
                     "examples"     -> examples.toString,
                     "successes"    -> successes.toString,
                     "expectations" -> expectations.toString,
                     "failures"     -> failures.toString,
                     "errors"       -> errors.toString,
                     "pending"      -> pending.toString,
                     "skipped"      -> skipped.toString,
                     "time"         -> timer.elapsed.toString)
    (stats /: attributes) { (res, cur) =>
      if (cur._2 == "0") res
      else            res % new UnprefixedAttribute(cur._1, cur._2, Null)
    }
  }

  override def toString =
    "Stats(examples = "     + examples     +", "+
           "successes = "   + successes    +", "+
           "expectations = "+ expectations +", "+
           "failures = "    + failures     +", "+
           "errors = "      + errors       +", "+
           "pending = "     + pending      +", "+
           "skipped = "     + skipped      +", "+
           "time = "        + timer.elapsed+")"

  /**
   * @return the "opposite" of this Stats object to be able to do subtractions
   */
  def negate =
    copy(
      examples     = -examples,
      successes    = -successes,
      expectations = -expectations,
      failures     = -failures,
      errors       = -errors,
      pending      = -pending,
      skipped      = -skipped
    )

  /**
   * @return this Statistics object with some trend if provided
   */
  def updateFrom(previous: Option[Stats]): Stats = (previous map updateFrom).getOrElse(this)
  /**
   * @return this Statistics object with some trend if relevant
   */
  def updateFrom(previous: Stats): Stats = {
    implicit val monoid = Stats.StatsMonoid
    val newTrend = this |+| previous.negate
    if (newTrend == monoid.zero) this
    else                         copy(trend = Some(newTrend))
  }

  /**
   * display the statistics on 2 lines, with the time and trend
   */
  def display(implicit args: Arguments) = {
    args.colors.stats("Finished in "+timer.time+"\n", args.color) +
    displayResults
  }

  /**
   * display the results on one line, always displaying examples/failures/errors
   * and only displaying expectations/pending/skipped if necessary
   */
  def displayResults(implicit args: Arguments) = {
    def trendIsDefined(f: Stats => Int) = trend map (t => f(t) != 0) getOrElse false

    def displayTrendValue(f: Stats => Int): String = {
			val i = trend map (t => f(t)) getOrElse 0
			if (i == 0) "" else if (i > 0) " (+"+i+")" else " ("+i+")"
		}

    def displayValue(f: Stats => Int, label: String, optional: Boolean = false, invariant: Boolean = false): Option[String] = {
      val base =
        if (optional && invariant) f(this) optInvariantQty label
        else if (optional)         f(this) optQty label
        else                       Some(f(this) qty label)
      base map (_ + displayTrendValue(f))
    }

    args.colors.stats(
        Seq(displayValue((_:Stats).examples, "example"),
            if (expectations != examples || trendIsDefined((_:Stats).expectations))
              displayValue((_:Stats).expectations, "expectation")
            else
              None,
            displayValue((_:Stats).failures, "failure"),
            displayValue((_:Stats).errors, "error"),
            displayValue((_:Stats).pending, "pending", optional = true),
            displayValue((_:Stats).skipped, "skipped", optional = true, invariant = true)
				).flatten.mkString(", "), args.color)

  }

}

/**
 * The Stats class store results for the number of:
 * * successes
 * * expectations
 * * failures
 * * errors
 * * pending
 * * skipped
 * 
 *  for each example
 *
 */
case object Stats {

  implicit object StatsMonoid extends Monoid[Stats] {
    def append(s1: Stats, s2: =>Stats) = {
      s1.copy(
        examples     = s1.examples        + s2.examples,
        successes    = s1.successes       + s2.successes,
        expectations = s1.expectations    + s2.expectations,
        failures     = s1.failures        + s2.failures,
        errors       = s1.errors          + s2.errors,
        pending      = s1.pending         + s2.pending,
        skipped      = s1.skipped         + s2.skipped,
        trend        = (s1.trend <**> s2.trend)(_ |+| _),
        timer        = s1.timer           add s2.timer
      )
    }

    val zero = Stats()
  }

  def apply(result: Result): Stats = 
    result match {
      case s @ Success(_)        => Stats(examples = 1, expectations = s.expectationsNb, successes = 1)
      case Failure(_, _, _, _)   => Stats(examples = 1, expectations = 1, failures = 1)
      case Error(_,_)            => Stats(examples = 1, expectations = 1, errors = 1)
      case Pending(_)            => Stats(examples = 1, expectations = 1, pending = 1)
      case Skipped(_, _)         => Stats(examples = 1, expectations = 1, skipped = 1)
      case DecoratedResult(t, r) => Stats(r)
    }
  
  def fromXml(stats: scala.xml.Node): Option[Stats] = {
    if (stats.label != Stats().toXml.label)
      None
    else {
      val map = stats.attributes.asAttrMap
      def asInt(key: String, defaultValue: Int = 0) = tryOrElse(Integer.parseInt(map(key)))(defaultValue)

      Some(Stats(asInt("examples"   ),
                 asInt("successes"   ),
                 asInt("expectations", 1),
                 asInt("failures"    ),
                 asInt("errors"      ),
                 asInt("pending"     ),
                 asInt("skipped"     ),
            (stats \ "trend" \ "stats").headOption.flatMap(fromXml),
            map.get("time").map(SimpleTimer.fromString).getOrElse(new SimpleTimer)))
    }

  }
}


