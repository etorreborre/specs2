package org.specs2
package specification

import control.Exceptions._
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
  def executeBody(body: =>Result): Result = tryOr(body)(Error(_))

  val executeFragment: Function[Fragment, ExecutedFragment] = { 
	  case e @ Example(s, _) =>     ExecutedResult(s, executeBody(e.execute))
	  case Text(s) =>               ExecutedText(s)
	  case Br() =>                  ExecutedBr()
	  case Par() =>                 ExecutedPar()
	  case End() =>                 ExecutedEnd()
	  case SpecStart(n) =>          ExecutedSpecStart(n)
	  case SpecEnd(n) =>            ExecutedSpecEnd(n)
    case s @ Step(a) => 
      executeBody(a()) match {
        case err @ Error(_, _) => ExecutedResult("action error", err)
        case _ =>                 ExecutedNoText()  
      }
    case _ =>                     ExecutedNoText()
  }

  /** this method is used in tests */
  def executeBodies(exs: Fragments): Seq[Result] = {
    exs.fragments.map(executeFragment(_)). collect { case r: ExecutedResult => r.result }
  }
}

