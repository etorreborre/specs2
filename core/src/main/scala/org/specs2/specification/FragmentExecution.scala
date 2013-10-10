package org.specs2
package specification

import control.Exceptions._
import text._
import main.Arguments
import time.SimpleTimer
import StandardFragments._
import execute._
import io.Location
import ResultLogicalCombinators._

/**
 * This trait executes Fragments
 *
 * It provides a function which executes fragments
 * and returns executed fragments
 */
trait FragmentExecution {

  /**
   * This method could be overriden to provide alternate behavior when executing an
   * Example
   */
  def executeBody(body: =>Result)(implicit arguments: Arguments): Result =
    if (arguments.plan)         Success("plan")
    else if (arguments.skipAll) Skipped()
    else                        ResultExecution.execute(body)

  /**
   * execute a Fragment.
   */
  def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = {
    val timer = new SimpleTimer().start
    (f: Fragment) => catchAllOr(execute(f))((e: Throwable) => ExecutedResult(FormattedString("Fragment evaluation error"), Error(e), timer.stop, f.location, Stats(Error(e))))
  }

  /**
   * execute a Fragment.
   *
   * A Form is executed separately by executing each row and cell, setting the results on each cell
   */
  def execute(f: Fragment)(implicit arguments: Arguments = Arguments()) = f match {
    case e @ Example(ff: FormFormattedString, _,_,_,_)     => {
      val timer = new SimpleTimer().start
      val executed = if (arguments.plan) ff.form else ff.form.executeForm
      val result = executed.execute
      ExecutedResult(FormFormattedString.create(executed), result, timer.stop, f.location, Stats(result).copy(timer = timer.stop))
    }
    case e: Example     => {
      val timer = new SimpleTimer().start
      val result = executeBody(e.execute)
      ExecutedResult(e.desc, result, timer.stop, f.location, Stats(result).copy(timer = timer.stop))
    }
    case t: Text                       => ExecutedText(t, f.location)
    case Br()                          => ExecutedBr(f.location)
    case Tab(n)                        => ExecutedTab(n, f.location)
    case Backtab(n)                    => ExecutedBacktab(n, f.location)
    case End()                         => ExecutedEnd(f.location)
    case s: SpecStart                  => ExecutedSpecStart(s.withArgs(arguments.overrideWith(s.arguments)), f.location, Stats().startTimer)
    case e: SpecEnd                    => ExecutedSpecEnd(e, f.location, Stats().startTimer)
    case s: Step                       => executeStep("step", s, f.location)
    case s: Action                     => executeStep("action", s, f.location)
    case other                         => executeStep("action", Action(), f.location)
  }

  private def executeStep(stepName: String, s: Fragment with Executable, location: Location)(implicit args: Arguments) = {
    val timer = new SimpleTimer().start
    executeBody(s.execute) match {
      case err if err.isError  => ExecutedResult(FormattedString(stepName+" error"), err, timer.stop, location, Stats(err))
      case f   if f.isFailure  => ExecutedResult(FormattedString(stepName+" failure"), f, timer.stop, location, Stats(f))
      case sk  @ Skipped(_, _) => ExecutedResult(FormattedString("skipped "+stepName), sk, timer.stop, location, Stats(sk))
      case other               => ExecutedNoText(s, new SimpleTimer, location)
    }
  }

  /** these methods are used in tests */
  private[specs2]
  def executeBodies(exs: Fragments)(implicit arguments: Arguments=Arguments()): Seq[Result] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r.result }
  }

  private[specs2]
  def executeExamples(exs: Fragments)(implicit arguments: Arguments=Arguments()): Seq[ExecutedResult] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r }
  }

  private[specs2]
  def executeExamplesResult(exs: Fragments)(implicit arguments: Arguments=Arguments()): Result =
    executeExamples(exs)(arguments).foldLeft(Success():Result) { (res, cur) => res and cur.result }

  private[specs2]
  def executeSpecificationResult(spec: SpecificationStructure)(implicit arguments: Arguments=Arguments()): Result =
    executeExamplesResult(spec.content)(arguments)
}

private[specs2]
object FragmentExecution extends FragmentExecution

