package org.specs2
package specification

import control.Exceptions._
import text._
import main.Arguments
import time.SimpleTimer
import execute._
import StandardFragments._

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
    else tryOr(body)(Error(_))
  }

  def executeFragment(implicit arguments: Arguments): Function[Fragment, ExecutedFragment] = { 
	  case e @ Example(s, _)     => ExecutedResult(s, executeBody(e.execute))
	  case Text(s)               => ExecutedText(s)
	  case Br()                  => ExecutedBr()
	  case Par()                 => ExecutedPar()
    case Tab(n)                => ExecutedTab(n)
    case Backtab(n)            => ExecutedBacktab(n)
	  case End()                 => ExecutedEnd()
	  case SpecStart(n, args)    => ExecutedSpecStart(n, new SimpleTimer().start, arguments.overrideWith(args))
	  case SpecEnd(n)            => ExecutedSpecEnd(n)
    case s @ Step(a)           => 
      executeBody(a()) match {
        case err @ Error(_, _) => ExecutedResult(NoMarkup("action error"), err)
        case _ =>                 ExecutedNoText()  
      }
    case _                     => ExecutedNoText()
  }

  /** this method is used in tests */
  def executeBodies(exs: Fragments)(implicit arguments: Arguments): Seq[Result] = {
    exs.fragments.map(f => executeFragment(arguments)(f)). collect { case r: ExecutedResult => r.result }
  }
}

