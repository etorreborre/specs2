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

/**
 * This trait executes Fragments
 *
 * It provides a function which executes fragments
 * and returns executed fragments
 */
private[specs2]
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

  def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = { 
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
    case s @ Step(a)           => 
      val timer = new SimpleTimer().start
      executeBody(s.execute) match {
        case err @ Error(_, _)    => ExecutedResult(NoMarkup("action error"), err, timer.stop)
        case f @ Failure(_, _, _) => ExecutedResult(NoMarkup("action failure"), f, timer.stop)
        case sk @ Skipped(_)      => ExecutedResult(NoMarkup("skipped action"), sk, timer.stop)
        case _ =>                    ExecutedNoText()
      }
    case See(link)                => ExecutedSee(link)
    case _                        => ExecutedNoText()
  }

  /** this method is used in tests */
  def executeBodies(exs: Fragments)(implicit arguments: Arguments): Seq[Result] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r.result }
  }
}

