package org.specs2
package runner
import specification._
import text.Plural._

trait NestedPrinter extends Printer with TotalStatistics with AConfiguration {

  val print: Function[(Int, Stats, ExecutedFragment), ExecutedFragment] = { p: (Int, Stats, ExecutedFragment) => 
	p match { 
  	  case (level, stats, ExecutedText(s)) => if (configuration.text) printText(level, s)
	  case (level, stats, ExecutedResult(s, result)) => printResult(level, s, result)
	  case (level, _, ExecutedPar()) => if (configuration.text) printPar(level)
	  case (level, stats, end @ ExecutedSpecEnd(_)) => printStats(level, stats, end)
	  case (level, stats, fragment) => if (configuration.text) printOther(level, stats, fragment)
    }
	p._3
  }
  def printText(level: Int, s: String)= println(("  " * level) + s)
  def printPar(level: Int) = println("")
  def printResult(level: Int, s: String, result: Result) = {
 	val description = "  " * level + status(result) + " " + s
    result match {
	  case e: HasStackTrace => {
	 	println(description)
	 	printWithLevel(level, result.message)
	 	if (configuration.printStackTrace)
	 	  e.stackTrace.foreach(t => printWithLevel(level, t.toString))
	  }
      case r: Success => if (configuration.text) println(description)
      case r: Pending => if (configuration.pending) println(description + " " + result.message)
      case r: Skipped => if (configuration.text) {
    	println(description)
	 	printWithLevel(level, result.message)
      }
    }
  }
  def printStats(level: Int, stats: Stats, result: ExecutedSpecEnd) = {
	(level, stats, result) match {   
      case (level, stats @ Stats(examples, expectations, failures, errors, pending, skipped), ExecutedSpecEnd(n)) => {
	    println("\nTotal for specification" + (if (n.isEmpty) n else " "+n))
	    println(
	      (List(
	        examples qty "example", 
	        expectations qty "expectation",
	        failures qty "failure",
	        errors qty "error") ++
	        List(pending.qty_>("pending")(0),
	             skipped.qty_>("skipped")(0)).flatten).mkString(", "))
	  }
	}
  }
  def printOther(level: Int, stats: Stats, fragment: ExecutedFragment) = ()
  
  def printWithLevel(level: Int, message: String) = println("  " * (level + 1) + message) 
  
  def status(result: Result) = {
	result match {
	  case Success(_) => "+"
	  case Failure(_) => "x"
	  case Error(_)   => "x"
	  case Pending(_) => "o"
	  case Skipped(_) => "o"
	}
  }
}
