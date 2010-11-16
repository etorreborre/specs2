package org.specs2
package reporter

import control.Throwablex._
import text.Plural._
import main.Arguments
import execute._
import specification._

/**
 * This trait prints the results of Fragments execution.
 * 
 * It also prints:
 *  * the description of the fragments
 *  * the statistics of the execution
 *    * the execution time
 *    * the number of examples, failures, errors,...
 *    
 * The behavior of this trait changes depending on the arguments:
 * 
 *   - xonly (default = false): if true print only errors and failures
 *   - failtrace (default = false): if true print the stacktrace of failures
 *    
 */
private[specs2]
trait TextPrinter extends ExecutedFragmentFold with ResultOutput {
  
  /**
   * Composition of statistics and a levels fold
   */
  val statistics  = new Statistics {}
  val levels      = new ExecutedLevelsFold {}
  val currentArgs = new ExecutedScopedArguments {}
  
  type T = (statistics.T, levels.T, currentArgs.T)
  def initial = (statistics.initial, levels.initial, currentArgs.initial)
  
  def fold(implicit args: Arguments) = (t: T, f: ExecutedFragment) => {
    (statistics.fold(args)(t._1, f), levels.fold(args)(t._2, f), currentArgs.fold(args)(t._3, f))
  }
  import statistics._
  
  
  /** print an ExecutedFragment and its associated statistics */
  def print(implicit arguments: Arguments): Function2[T, ExecutedFragment, ExecutedFragment] = { 
    (t: T, f: ExecutedFragment) => (t, f) match { 
      case ((stats, level, args), ExecutedSpecStart(s, _, _))=> printSpecStart(s, level)(args)
      case ((stats, level, args), ExecutedResult(s, result)) => printResult(s, level, result)(args)
      case ((stats, level, args), ExecutedText(s)) =>           printExecutedText(s, level)(args)
      case ((stats, level, args), ExecutedPar()) =>             printExecutedPar(args)
      case (((total, current), level, args), 
            end @ ExecutedSpecEnd(_)) =>                        printExecutedEnd(total, current, level, end)(args)
	    case (s @ (stats, level, args), fragment) =>              printOther(s, fragment)(args)
    }
	  f
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
    if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) 
      printEndStats(current, end)
    if (total.isEnd(end))
      printEndStats(total, end)
  }
  
  
  def leveledText(s: String, level: levels.T)(implicit args: Arguments): String = { 
    if (args.noindent) s 
   else (("  " * level.level) + s.trim)
  }
  
  /** print a piece of text */
  def printText(s: String)(implicit args: Arguments) = printMessage(s)
  /** print a paragraph */
  def printPar()(implicit args: Arguments) = printLine(" ")
  
  def printResult(desc: String, result: Result)(implicit args: Arguments): Unit = {
	  val description = statusAndDescription(desc, result)
    result match {
	    case f: Failure => {
        printFailureOrError(desc, f) 
	 	    if (args.failtrace) 
	 	      f.stackTrace.foreach(t => printError(t.toString))
	    }
      case e: Error => {
        printFailureOrError(desc, e) 
        e.stackTrace.foreach(t => printError(t.toString))
        e.exception.chainedExceptions.foreach { (t: Throwable) =>
          printError(t.getMessage)
          t.getStackTrace.foreach(st => printError(st.toString))
        }
      }
      case Success(_) => if (!args.xonly) printSuccess(description)
      case Pending(_) => if (!args.xonly) printPending(description + " " + result.message)
      case Skipped(_) => if (!args.xonly) {
      	printSkipped(description)
	 	    printSkipped(result.message)
      }
    }
  }
  def printFailureOrError(desc: String, f: Result with ResultStackTrace)(implicit args: Arguments) = { 
    val description = statusAndDescription(desc, f)
    printError(description)
    printError(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
  }
  def printEndStats(stats: Stats, result: ExecutedSpecEnd)(implicit args: Arguments) = (stats, result) match {   
    case (stats, ExecutedSpecEnd(n)) => printSpecStats(stats, n)
  }
  def printSpecStats(stats: Stats, name: String)(implicit args: Arguments) = {
    printLine("\nTotal for specification" + (if (name.isEmpty) name.trim else " "+name.trim))
    printStats(stats)
    printLine(" ")
  }
  def printStats(stats: Stats)(implicit args: Arguments) = {
    val Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart, specEnd) = stats
    stats.start.map(s => printLine("Finished in " + s.timer.time))
    printLine(
        Seq(Some(examples qty "example"), 
            if (expectations != examples) Some(expectations qty "expectation") else None,
            Some(failures qty "failure"), 
            Some(errors qty "error"),
            pending optQty "pending", 
            skipped optQty "skipped").flatten.mkString(", "))
  }
  
  /** don't print anything for now */
  def printOther(stats: T, fragment: ExecutedFragment)(implicit args: Arguments) = ()
  def statusAndDescription(s: String, result: Result)(implicit args: Arguments) = {
    s.takeWhile(_ == ' ') + status(result) + s.dropWhile(_ == ' ')
  }
  def status(result: Result)(implicit args: Arguments): String = {
    if (args.plan) ""
    else (result.status  + " ")
  }
}
