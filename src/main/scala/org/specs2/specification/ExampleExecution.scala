package org.specs2
package specification
import execute._
import control.Exceptions._

trait AnExecutor {
  val executor: ExampleExecution = new ExampleExecution {}
}
/**
 * This trait executes examples
 *
 */
trait ExampleExecution {
  import StandardFragments._
  
  def executeBody(body: =>Result): Result = tryOr(body)(Error(_))

  def executeBodies(exs: Examples): List[Result] = {
    ((Nil:List[Result]) /: exs.fragments) { (res: List[Result], e: Fragment) =>
      execute(e) match {
    	case r: ExecutedResult => res :+ r.result 
    	case _ => res
      }
    }
  }
  
  val execute: Function[Fragment, ExecutedFragment] = { 
	case e @ Example(s, _) => ExecutedResult(s, executeBody(e.execute))
	case s @ Step(a) => executeBody(a()) match {
	  case err @ Error(_, _) => ExecutedResult("action error", err)
	  case _ => ExecutedNoText()	
	}
	case Text(s) => ExecutedText(s)
	case Br() => ExecutedBr()
	case Par() => ExecutedPar()
	case SpecStart(n) => ExecutedSpecStart(n)
	case SpecEnd(n) => ExecutedSpecEnd(n)
	case f => ExecutedNoText()
  }
}

