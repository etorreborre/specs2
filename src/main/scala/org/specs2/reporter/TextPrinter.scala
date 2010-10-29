package org.specs2
package reporter
import specification._
import execute._
import text.Plural._

trait TextPrinter extends Printer with TotalStatistics with AConfiguration {

  def print(implicit args: Args): Function[(Stats, ExecutedFragment), ExecutedFragment] = { p: (Stats, ExecutedFragment) => 
	  p match { 
      case (stats, ExecutedSpecStart(s)) => printText(s)
  	  case (stats, ExecutedText(s)) => if (!args.xonly) printText(s)
	    case (stats, ExecutedResult(s, result)) => printResult(s, result)
	    case (_, ExecutedPar()) => if (!args.xonly) printPar()
	    case (stats, end @ ExecutedSpecEnd(_)) => printStats(stats, end)
	    case (stats, fragment) => if (!args.xonly) printOther(stats, fragment)
    }
	  p._2
  }
  def printText(s: String)= println(s)
  def printPar() = println(" ")
  def statusAndDescription(s: String, result: Result) = {
	  s.takeWhile(_ == ' ') + status(result) + " " + s.dropWhile(_ == ' ')
  }
  def printResult(s: String, result: Result)(implicit args: Args) = {
	val description = statusAndDescription(s, result)
    result match {
	    case e: ResultStackTrace => {
	 	    printMessage(description)
	 	    printMessage(s.takeWhile(_ == ' ') + "  " + result.message + " ("+e.location+")")
	 	    if (configuration.printStackTrace)
	 	      e.stackTrace.foreach(t => printMessage(t.toString))
	      }
      case Success(_) => if (!args.xonly) printMessage(description)
      case Pending(_) => if (!args.xonly) printMessage(description + " " + result.message)
      case Skipped(_) => if (!args.xonly) {
      	printMessage(description)
	 	    printMessage(result.message)
      }
    }
  }
  def printStats(stats: Stats, result: ExecutedSpecEnd) = {
	  (stats, result) match {   
        case (stats @ Stats(examples, expectations, failures, errors, pending, skipped), ExecutedSpecEnd(n)) => {
	        println("\nTotal for specification" + (if (n.isEmpty) n else " "+n))
	        println((
	                 List(examples qty "example") ++ 
	                 (if (expectations != examples) List(expectations qty "expectation") else Nil) ++
	                 List(failures qty "failure", errors qty "error") ++
	                 List(pending.qty_>("pending")(0), skipped.qty_>("skipped")(0)).flatten).mkString(", "))
	        println(" ")
	    }
	  }
  }
  def printOther(stats: Stats, fragment: ExecutedFragment) = ()
  
  def printMessage(message: String) = {
	  val splitted = message.split("\n")
	  if (splitted.size > 1) splitted.foreach(m => println(m))
	  else println(message)
  }
  
  def status(result: Result) = result.status
}
