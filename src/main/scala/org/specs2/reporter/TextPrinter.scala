package org.specs2
package reporter

import text.Plural._
import main.Arguments
import execute._
import specification._

/**
 * This trait prints the result of the Fragment execution to the Printer output
 * It also prints statistics at the end of the execution
 */
private[specs2]
trait TextPrinter extends ExecutedFragmentFold with PrintMessages {
  
  val statistics = new Statistics {}
  val levels     = new ExecutedLevelsFold {}
  type T = (statistics.T, levels.T)
  def initial = (statistics.initial, levels.initial)
  def fold(implicit args: Arguments) = (t: T, f: ExecutedFragment) => {
    (statistics.fold(args)(t._1, f), levels.fold(args)(t._2, f))
  }
  import statistics._
  
  
  /** print an ExecutedFragment and its associated statistics */
  def print(implicit args: Arguments): Function[(T, ExecutedFragment), ExecutedFragment] = { p: (T, ExecutedFragment) => 
	  p match { 
      case ((stats, level), ExecutedSpecStart(s))      => printSpecStart(s, level)
      case ((stats, level), ExecutedResult(s, result)) => printResult(s, level, result)
      case ((stats, level), ExecutedText(s)) =>           printExecutedText(s, level)
      case (_, ExecutedPar()) =>                          printExecutedPar
      case (((total, current), level), 
            end @ ExecutedSpecEnd(_)) =>                  printExecutedEnd(total, current, level, end)
	    case (stats, fragment) =>                           printOther(stats, fragment)
    }
	  p._2
  }
  
  def printSpecStart(s: String, level: levels.T)(implicit args: Arguments) = {
    printText(leveledText(s, level))
  }
  def printResult(s: String, level: levels.T, result: Result)(implicit args: Arguments): Unit = 
    printResult(leveledText(s, level), result)
    
  def printExecutedText(s: String, level: levels.T)(implicit args: Arguments) = {
    if (!args.xonly) printText(leveledText(s, level))
  }
  def printExecutedPar(implicit args: Arguments) = {
    if (!args.xonly) printPar()
  }
  def printExecutedEnd(total: Stats, current: Stats, level: levels.T, end: ExecutedSpecEnd)(implicit args: Arguments) = {
    if (!args.xonly || current.hasFailuresOrErrors) {
      if (!total.isEnd(end)) printEndStats(current, end)
      else printEndStats(total, end)
    }
  }
  
  
  def leveledText(s: String, level: levels.T): String = ("  " * level.level) + s.trim
  /** print a piece of text */
  def printText(s: String)= printLine(s)
  /** print a paragraph */
  def printPar() = printLine(" ")
  
  def printResult(s: String, result: Result)(implicit args: Arguments): Unit = {
	  val description = statusAndDescription(s, result)
    result match {
	    case e: ResultStackTrace => {
	 	    printError(description)
	 	    printError(s.takeWhile(_ == ' ') + "  " + result.message + " ("+e.location+")")
	 	    if (args.printStackTrace) 
	 	      e.stackTrace.foreach(t => printError(t.toString))
	    }
      case Success(_) => if (!args.xonly) printSuccess(description)
      case Pending(_) => if (!args.xonly) printPending(description + " " + result.message)
      case Skipped(_) => if (!args.xonly) {
      	printSkipped(description)
	 	    printSkipped(result.message)
      }
    }
  }
  def printEndStats(stats: Stats, result: ExecutedSpecEnd)(implicit args: Arguments) = (stats, result) match {   
    case (stats, ExecutedSpecEnd(n)) => printSpecStats(stats, n)
  }
  def printSpecStats(stats: Stats, name: String)(implicit args: Arguments) = {
    printLine("\nTotal for specification" + (if (name.isEmpty) name.trim else " "+name.trim))
    printStats(stats)
    printLine(" ")
  }
  def printStats(stats: Stats) = {
    val statistics.Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart) = stats
    printLine((
                 List(examples qty "example") ++ 
                 (if (expectations != examples) List(expectations qty "expectation") else Nil) ++
                 List(failures qty "failure", errors qty "error") ++
                 List(pending optQty "pending", skipped optQty "skipped").flatten).mkString(", "))
  }
  
  /** don't print anything for now */
  def printOther(stats: T, fragment: ExecutedFragment)(implicit args: Arguments) = ()
  def statusAndDescription(s: String, result: Result) = {
    s.takeWhile(_ == ' ') + status(result) + " " + s.dropWhile(_ == ' ')
  }
  def status(result: Result): String = result.status
}
