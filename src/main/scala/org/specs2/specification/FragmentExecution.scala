package org.specs2
package specification

import control.Exceptions._
import text._
import main.Arguments
import time.SimpleTimer
import execute._
import StandardFragments._
import matcher.FailureException
import junit.framework.AssertionFailedError
import form.Form
import scala.Either

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
  def executeBody(body: =>Result)(implicit arguments: Arguments): Result = {
    if (arguments.plan) Success("plan")
    else try {
      body
    } catch {
      case FailureException(f) => f
      case e: Exception        => Error(e)
      case other               => throw other
    }
  }

  /**
   * execute a Fragment.
   */
  def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = {
    val timer = new SimpleTimer().start
    (f: Fragment) => tryOr(execute(f))((e: Exception) => ExecutedResult(NoMarkup("Fragment evaluation error"), Error(e), timer.stop))
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
      ExecutedResult(FormMarkup(executed), executed.execute, timer.stop)
    }
	  case e @ Example(s, _)     => {
      val timer = new SimpleTimer().start
      ExecutedResult(s, executeBody(e.execute), timer.stop)
    }
	  case Text(s)               => ExecutedText(s)
	  case Br()                  => ExecutedBr()
    case Tab(n)                => ExecutedTab(n)
    case Backtab(n)            => ExecutedBacktab(n)
	  case End()                 => ExecutedEnd()
	  case SpecStart(n, args)    => ExecutedSpecStart(n, arguments.overrideWith(args))
	  case SpecEnd(n)            => ExecutedSpecEnd(n)
    case s @ Step(_)           => executeStep("step", s)
    case s @ Action(_)         => executeStep("action", s)
    case See(link)             => ExecutedSee(link)
    case _                     => ExecutedNoText()
  }

  private def executeStep(stepName: String, s: Executable)(implicit args: Arguments) = {
    val timer = new SimpleTimer().start
    executeBody(s.execute) match {
      case err @ Error(_, _)       => ExecutedResult(NoMarkup(stepName+" error"), err, timer.stop)
      case f @ Failure(_,_ , _, _) => ExecutedResult(NoMarkup(stepName+" failure"), f, timer.stop)
      case sk @ Skipped(_, _)      => ExecutedResult(NoMarkup("skipped "+stepName), sk, timer.stop)
      case _                       => ExecutedNoText()
    }
  }

  /** this method is used in tests */
  def executeBodies(exs: Fragments)(implicit arguments: Arguments): Seq[Result] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r.result }
  }
}

