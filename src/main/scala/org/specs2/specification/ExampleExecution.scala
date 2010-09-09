package org.specs2
package specification
trait AnExecutor {
  val executor: ExampleExecution = new ExampleExecution {}
}
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
	case SpecStart(n) => ExecutedSpecStart(n)
	case SpecEnd(n) => ExecutedSpecEnd(n)
	case _ => ExecutedNoText()
  }
}
sealed trait ExecutedFragment
case class ExecutedText(s: String) extends ExecutedFragment
case class ExecutedResult(s: String, r: Result) extends ExecutedFragment
case class ExecutedBr() extends ExecutedFragment
case class ExecutedPar() extends ExecutedFragment
case class ExecutedSpecStart(n: String) extends ExecutedFragment
case class ExecutedSpecEnd(n: String) extends ExecutedFragment
case class ExecutedNoText() extends ExecutedFragment

