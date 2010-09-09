package org.specs2
package runner
import specification._
import io._

trait APrinter extends Output { outer =>
  val printer: Printer = new NestedPrinter {
	val output = outer
  }
}
trait Printer {
  val output: Output
  def println(s: String) = output.println(s)
  val print: Function[(Int, ExecutedFragment), Unit]
}
trait NestedPrinter extends Printer {

  val print: Function[(Int, ExecutedFragment), Unit] = { 
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
