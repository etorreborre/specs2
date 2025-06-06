package org.specs2
package specification
package process

import main.Arguments
import execute.*
import org.specs2.fp.*
import org.specs2.fp.syntax.*
import execute.StandardResults
import text.*
import Plural.*
import control.Exceptions.*
import time.SimpleTimer

/** The Stats class store results for the number of:
  *
  *   - linked specifications
  *   - examples (including linked examples)
  *   - successes
  *   - expectations
  *   - failures
  *   - errors
  *   - pending
  *   - skipped
  */
case class Stats(
    specs: Int = 0,
    examples: Int = 0,
    successes: Int = 0,
    expectations: Int = 0,
    failures: Int = 0,
    errors: Int = 0,
    pending: Int = 0,
    skipped: Int = 0,
    trend: Option[Stats] = None,
    timer: SimpleTimer = new SimpleTimer
):

  /** @return true if there are errors or failures */
  def hasFailuresOrErrors = failures + errors > 0

  /** @return true if there are expectations */
  def hasExpectations = expectations > 0

  /** @return an equivalent result for display */
  def result =
    if failures + errors == 0 then
      if successes > 0 || skipped + pending == 0 then StandardResults.success
      else if pending > skipped then StandardResults.pending
      else StandardResults.skipped
    else if errors > 0 then StandardResults.anError
    else StandardResults.failure

  /** @return true if there are no issues at all */
  def isSuccess = result.isSuccess

  /** @return true if there are failures or errors */
  def hasIssues = result.isFailure || result.isError

  /** @return true if there are skipped or pending */
  def hasSuspended = result.isSkipped || result.isPending

  /** @return true if there are failures */
  def hasFailures = result.isFailure

  /** @return true if there are errors */
  def hasErrors = result.isError

  /** @return the execution time */
  def time = timer.time

  /** @return the same stats but with a started timer */
  def startTimer = copy(timer = timer.start)

  /** set a specific result on this Stats object */
  def withResult(result: Result): Stats =
    result match
      case s @ Success(_, _)       => copy(expectations = result.expectationsNb, successes = 1)
      case f @ Failure(_, _, _, _) => copy(expectations = result.expectationsNb, failures = 1)
      case e @ Error(_, _)         => copy(expectations = result.expectationsNb, errors = 1)
      case Pending(_)              => copy(expectations = result.expectationsNb, pending = 1)
      case Skipped(_, _)           => copy(expectations = result.expectationsNb, skipped = 1)
      case DecoratedResult(t, r)   =>
        t.asInstanceOf[Matchable] match
          case s: Stats => s
          case _        => withResult(r)

  override def toString =
    "Stats(" +
      "specs = " + specs + ", " +
      "examples = " + examples + ", " +
      "successes = " + successes + ", " +
      "expectations = " + expectations + ", " +
      "failures = " + failures + ", " +
      "errors = " + errors + ", " +
      "pending = " + pending + ", " +
      "skipped = " + skipped + ", " +
      "time = " + timer.totalMillis + ")"

  /** @return
    *   the "opposite" of this Stats object to be able to do subtractions
    */
  def negate =
    copy(
      specs = -specs,
      examples = -examples,
      successes = -successes,
      expectations = -expectations,
      failures = -failures,
      errors = -errors,
      pending = -pending,
      skipped = -skipped
    )

  /** @return
    *   this Statistics object with some trend if provided
    */
  def updateFrom(previous: Option[Stats]): Stats = (previous map updateFrom).getOrElse(this)

  /** @return
    *   this Statistics object with some trend if relevant
    */
  def updateFrom(previous: Stats): Stats =
    given monoid: Monoid[Stats] = Stats.StatsMonoid
    val newTrend = this |+| previous.negate
    if newTrend == monoid.zero then this
    else copy(trend = Some(newTrend))

  /** display the statistics on 2 lines, with the time and trend
    */
  def display(using args: Arguments) =
    args.colors.stats("Finished in " + timer.time + "\n", args.color) +
      displayResults

  /** display the results on one line, always displaying examples/failures/errors and only displaying
    * expectations/pending/skipped if necessary
    */
  def displayResults(using args: Arguments) =
    def trendIsDefined(f: Stats => Int) = trend map (t => f(t) != 0) getOrElse false

    def displayTrendValue(f: Stats => Int): String =
      val i = trend map (t => f(t)) getOrElse 0
      if i == 0 then "" else if i > 0 then " (+" + i + ")" else " (" + i + ")"

    def displayValue(
        f: Stats => Int,
        label: String,
        optional: Boolean = false,
        invariant: Boolean = false
    ): Option[String] =
      val base =
        if optional && invariant then f(this) `optInvariantQty` label
        else if optional then f(this) `optQty` label
        else Some(f(this) `qty` label)
      base map (_ + displayTrendValue(f))

    args.colors.stats(
      Seq(
        displayValue((_: Stats).specs, "specification", optional = true),
        displayValue((_: Stats).examples, "example"),
        if expectations != examples || trendIsDefined((_: Stats).expectations) then
          displayValue((_: Stats).expectations, "expectation")
        else None,
        displayValue((_: Stats).failures, "failure"),
        displayValue((_: Stats).errors, "error"),
        displayValue((_: Stats).pending, "pending", optional = true, invariant = true),
        displayValue((_: Stats).skipped, "skipped", optional = true, invariant = true)
      ).flatten.mkString(", "),
      args.color
    )

/** The Stats class store results for the number of:
  *   - successes
  *   - expectations
  *   - failures
  *   - errors
  *   - pending
  *   - skipped
  *
  * for each example
  */
case object Stats:

  def empty = Stats()

  implicit object StatsMonoid extends Monoid[Stats]:
    def append(s1: Stats, s2: =>Stats) =
      s1.copy(
        specs = s1.specs + s2.specs,
        examples = s1.examples + s2.examples,
        successes = s1.successes + s2.successes,
        expectations = s1.expectations + s2.expectations,
        failures = s1.failures + s2.failures,
        errors = s1.errors + s2.errors,
        pending = s1.pending + s2.pending,
        skipped = s1.skipped + s2.skipped,
        trend = Applicative[Option].apply2(s1.trend, s2.trend)(_ |+| _),
        timer = s1.timer `add` s2.timer
      )

    val zero = empty

  def apply(result: Result): Stats =
    result match
      case s @ Success(_, _)       => Stats(examples = 1).withResult(result)
      case f @ Failure(_, _, _, _) => Stats(examples = 1).withResult(result)
      case e @ Error(_, _)         => Stats(examples = 1).withResult(result)
      case Pending(_)              => Stats(examples = 1).withResult(result)
      case Skipped(_, _)           => Stats(examples = 1).withResult(result)
      case DecoratedResult(t, r)   =>
        t.asInstanceOf[Matchable] match
          case s: Stats => s
          case other    => Stats(r)
