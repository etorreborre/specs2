package org.specs2
package specification

import control.Exceptions._
import text._
import main.Arguments
import time.SimpleTimer
import StandardFragments._
import junit.framework.AssertionFailedError
import form.Form
import scala.Either
import execute._
import io.Location

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
    if (arguments.plan) Success("plan")
    else if (arguments.skipAll) Skipped()
    else ResultExecution.execute(body)

  /**
   * execute a Fragment.
   */
  def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = {
    val timer = new SimpleTimer().start
    (f: Fragment) => catchAllOr(execute(f))((e: Throwable) => ExecutedResult(NoMarkup("Fragment evaluation error"), Error(e), timer.stop, f.location))
  }

  /**
   * execute a Fragment.
   *
   * A Form is executed separately by executing each row and cell, setting the results on each cell
   */
  protected def execute(f: Fragment)(implicit arguments: Arguments) = f match {
    case Example(FormMarkup(form), _)     => {
      val timer = new SimpleTimer().start
      val executed = if (arguments.plan) form else form.executeForm
      ExecutedResult(FormMarkup(executed), executed.execute, timer.stop, f.location)
    }
	  case e @ Example(s, _)     => {
      val timer = new SimpleTimer().start
      ExecutedResult(s, executeBody(e.execute), timer.stop, f.location)
    }
	  case Text(s)               => ExecutedText(s, f.location)
	  case Br()                  => ExecutedBr(f.location)
    case Tab(n)                => ExecutedTab(n, f.location)
    case Backtab(n)            => ExecutedBacktab(n, f.location)
	  case End()                 => ExecutedEnd(f.location)
	  case SpecStart(n, args)    => ExecutedSpecStart(n, arguments.overrideWith(args), f.location)
	  case SpecEnd(n)            => ExecutedSpecEnd(n, f.location)
    case s @ Step(_)           => executeStep("step", s, f.location)
    case s @ Action(_)         => executeStep("action", s, f.location)
    case See(link)             => ExecutedSee(link, f.location)
    case _                     => ExecutedNoText(new SimpleTimer, f.location)
  }

  private def executeStep(stepName: String, s: Executable, location: Location)(implicit args: Arguments) = {
    val timer = new SimpleTimer().start
    executeBody(s.execute) match {
      case err @ Error(_, _)         => ExecutedResult(NoMarkup(stepName+" error"), err, timer.stop, location)
      case f   @ Failure(_,_ , _, _) => ExecutedResult(NoMarkup(stepName+" failure"), f, timer.stop, location)
      case sk  @ Skipped(_, _)       => ExecutedResult(NoMarkup("skipped "+stepName), sk, timer.stop, location)
      case _                         => ExecutedNoText(new SimpleTimer, location)
    }
  }

  /** this method is used in tests */
  def executeBodies(exs: Fragments)(implicit arguments: Arguments=Arguments()): Seq[Result] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r.result }
  }
}
private[specs2]
object FragmentExecution extends FragmentExecution

