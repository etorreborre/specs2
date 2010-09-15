package org.specs2
package specification
import execute._

trait AnExecutor {
  val executor: ExampleExecution = new ExampleExecution {}
}
trait ExampleExecution extends PredefinedFragments {
  def executeBody(body: =>Result): Result = {
	try {
	  body
	} catch {
	  case e: Exception => Error(e)
	}
  }
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
	  case err @ Error(_) => ExecutedResult("action error", err)
	  case _ => ExecutedNoText()	
	}
	case Text(s) => ExecutedText(s)
	case `br` => ExecutedBr()
	case `par` => ExecutedPar()
	case SpecStart(n) => ExecutedSpecStart(n)
	case SpecEnd(n) => ExecutedSpecEnd(n)
	case _ => ExecutedNoText()
  }
}
sealed trait ExecutedFragment
case class ExecutedText(text: String) extends ExecutedFragment
case class ExecutedResult(text: String, result: Result) extends ExecutedFragment
case class ExecutedBr() extends ExecutedFragment
case class ExecutedPar() extends ExecutedFragment
case class ExecutedSpecStart(name: String) extends ExecutedFragment
case class ExecutedSpecEnd(name: String) extends ExecutedFragment
case class ExecutedNoText() extends ExecutedFragment

