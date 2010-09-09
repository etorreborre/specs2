package org.specs2
package specification

trait ExampleExecution {
  def execute(body: () => Result): Result = {
	try {
	  body() 
	} catch {
	  case e: Exception => Error(e.getMessage + e.getStackTraceString)
	}
  }
  val execute: PartialFunction[Fragment, ExecutedFragment] = { 
	case Text(s) => ExecutedText(s)
	case e @ Example(s, Some(body)) => ExecutedResult(s, execute(body))
	case `br` => ExecutedBr()
	case `par` => ExecutedPar()
	case `end` => ExecutedEnd()
	case _ => ExecutedNoText()
  }
}
sealed trait ExecutedFragment
case class ExecutedText(s: String) extends ExecutedFragment
case class ExecutedResult(s: String, r: Result) extends ExecutedFragment
case class ExecutedBr() extends ExecutedFragment
case class ExecutedPar() extends ExecutedFragment
case class ExecutedEnd() extends ExecutedFragment
case class ExecutedNoText() extends ExecutedFragment

