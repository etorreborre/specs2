package org.specs2
package runner
import specification._
import io._

trait NestedPrinter extends Output {

  val print: PartialFunction[(Int, ExecutedFragment), Unit] = { 
	case (level, ExecutedText(s)) => println(("  " * level) + s)
	case (level, ExecutedResult(s, result)) => {
	  println("  " * level + status(result) + " " + s)
	  printMessage(level + 1, result)
	}
	case (_, ExecutedPar()) => println("")
	case _ => ()
  }
  
  private def printMessage(level: Int, result: Result) = {
	result match {
	  case Success(_) => ()
	  case _ => println("  " * level + result.message)
	}
  }
  private def status(result: Result) = {
	result match {
	  case Success(_) => "+"
	  case Failure(_) => "x"
	  case Error(_)   => "x"
	  case Pending(_) => "o"
	  case Skipped(_) => "o"
	}
  }
}
