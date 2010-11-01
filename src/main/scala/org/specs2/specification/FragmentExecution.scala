package org.specs2
package specification

import control.Exceptions._
import execute._

/**
 * This trait provides an Executor with a default implementation
 */
private[specs2]
trait AnExecutor {
  val executor: FragmentExecution = new FragmentExecution {}
}
/**
 * This trait executes Fragments
 *
 * It provides a method which executes fragments
 * and returns executed fragments
 */
private[specs2]
trait FragmentExecution {
  import StandardFragments._
  
  def executeBody(body: =>Result): Result = tryOr(body)(Error(_))

  val executeFragment: Function[Fragment, ExecutedFragment] = { 
	  case e @ Example(s, _) =>     ExecutedResult(s, executeBody(e.execute))
	  case s @ Step(a) => 
	    executeBody(a()) match {
	      case err @ Error(_, _) => ExecutedResult("action error", err)
	      case _ =>                 ExecutedNoText()	
	    }
	  case Text(s) =>               ExecutedText(s)
	  case Br() =>                  ExecutedBr()
	  case Par() =>                 ExecutedPar()
	  case SpecStart(n) =>          ExecutedSpecStart(n)
	  case SpecEnd(n) =>            ExecutedSpecEnd(n)
	  case f =>                     ExecutedNoText()
  }

  private[specs2]
  def executeBodies(exs: Fragments): List[Result] = {
    exs.fragments.map(executeFragment(_)). collect { case r: ExecutedResult => r.result }
  }
  
}

