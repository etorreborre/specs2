package org.specs2
package reporter

import scalaz.{ Monoid, Reducer, Scalaz, Generator, Foldable }
import Generator._
import control.Throwablex._
import data.Tuples._
import time._
import text._
import Plural._
import AnsiColors._
import NotNullStrings._
import EditDistance._
import DiffShortener._
import execute._
import main.Arguments
import specification._
import Statistics._
import Levels._
import SpecsArguments._

/**
 * This trait reduces a list of ExecutedFragments to a list of PrintLines.
 * 
 * Each line contains:
 * * A description (text or example description)
 * * A level, to work out the indenting
 * * Some statistics, to print on SpecEnd
 * * The current arguments, to control the conditional printing of text, statistics,...
 *
 */
trait TextPrinter {
  val output: ResultOutput = new TextResultOutput
  
  def print(s: SpecificationStructure, fs: Seq[ExecutedFragment])(implicit args: Arguments) =
    printLines(fs).print(output)
  
  def printLines(fs: Seq[ExecutedFragment]) = 
    PrintLines(flatten(FoldrGenerator[Seq].reduce(reducer, fs)))
  
  private  val reducer = 
    PrintReducer &&& 
    StatisticsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer
  
  case class PrintLine(text: Print = PrintBr(), stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) {
    def print(implicit out: ResultOutput) = text.print(stats, level, args)
  }
  
  case class PrintLines(lines : List[PrintLine] = Nil) {
    def print(implicit out: ResultOutput) = lines foreach (_.print)
  }
  
  def flatten(results: (((List[Print], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[PrintLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.totals zip levels.levels zip args.toList) map {
      case (((t, s), l), a) => PrintLine(t, s, l, a)
    }
  }  
    
  implicit object PrintReducer extends Reducer[ExecutedFragment, List[Print]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_, _)     => PrintSpecStart(start)
      case result @ ExecutedResult(_, _, _)    => PrintResult(result)
      case text @ ExecutedText(s)              => PrintText(text)
      case par @ ExecutedBr()                  => PrintBr()
      case end @ ExecutedSpecEnd(_)            => PrintSpecEnd(end)
      case fragment                            => PrintOther(fragment)
    }
  }
    
  sealed trait Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput): Unit
    
    /**
     * indent the text to the wanted level.
     * If the text contains several lines, each line is indented
     */
    protected def leveledText(s: String, level: Int)(implicit args: Arguments): String = { 
      if (args.noindent) s 
      else {
        val indent = "  "*level
        s.trim.split("\n").map(indent+_).mkString("\n") + (if (args.showlevel) " ("+level+")" else "")
      }
    }
  }
  case class PrintSpecStart(start: ExecutedSpecStart) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      out.printSpecStart(leveledText(start.name.name, level)(args))(args)
    } 
  }
  case class PrintResult(r: ExecutedResult)           extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      printResult(leveledText(r.text(args).toString, level)(args), r.result, r.timer)(args, out)
      
    def printResult(desc: String, result: Result, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput): Unit = {
      val description = statusAndDescription(desc, result, timer)(args, out)
      result match {
        case f @ Failure(m, e, st, d) => {
          printFailure(desc, f, timer)
          printFailureDetails(d)
        }
        case e: Error => {
          printError(desc, e, timer)
          e.stackTrace.foreach(t => out.printError(t.toString))
          e.exception.chainedExceptions.foreach { (t: Throwable) =>
            out.printError(t.getMessage.notNull)
            args.traceFilter(t.getStackTrace.toSeq).foreach(st => out.printError(st.toString))
          }
        }
        case Success(_)    => if (!args.xonly) out.printSuccess(description)
        case Pending(_)    => if (!args.xonly) out.printPending(description + " " + result.message)
        case Skipped(_, _) => if (!args.xonly) {
          out.printSkipped(description)
          if (!result.message.isEmpty)
            out.printSkipped(result.message)
        }
      }
    }
    def printFailure(desc: String, f: Result with ResultStackTrace, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer)
      out.printFailure(description)
      out.printFailure(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
      if (args.failtrace)
        f.stackTrace.foreach(t => out.printFailure(t.toString))
    }
    def printFailureDetails(d: Details)(implicit args: Arguments, out: ResultOutput) = {
      d match {
        case FailureDetails(expected, actual) if (args.diffs.show(expected, actual)) => {
          val (expectedDiff, actualDiff) = showDistance(expected, actual, args.diffs.separators, args.diffs.shortenSize)
          out.printFailure("Expected: " + expectedDiff)
          out.printFailure("Actual:   " + actualDiff)
          if (args.diffs.full) {
            out.printFailure("Expected (full): " + expected)
            out.printFailure("Actual (full):   " + actual)
          }
          out.printLine("")
        }
        case _ => ()
      }
    }
    def printError(desc: String, f: Result with ResultStackTrace, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer)
      out.printError(description)
      out.printError(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
    }
    /**
     * add the status to the description
     * making sure that the description is still properly aligned, even with several lines
     */
    def statusAndDescription(text: String, result: Result, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput) = {
      val textLines = text.split("\n")
      def time = if (args.showtimes) " ("+timer.time+")" else ""
      val firstLine = textLines.take(1).map { s =>
        s.takeWhile(_ == ' ').dropRight(2) +
        out.status(result)(args) + s.dropWhile(_ == ' ') + time
      }
      val rest = textLines.drop(1)
      (firstLine ++ rest).mkString("\n")
    }
  }
  case class PrintText(t: ExecutedText)               extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (!args.xonly) 
        out.printMessage(leveledText(t.text, level)(args))(args)
  }        
  case class PrintBr()                               extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (!args.xonly) out.printLine(" ")(args)
  }
  case class PrintSpecEnd(end: ExecutedSpecEnd)       extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      if (!stats.isEnd(end) && args.xonly && stats.hasFailuresOrErrors)
        printEndStats(stats)(args, out)
      if (stats.isEnd(end))
        printEndStats(stats)(args, out)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      val n = end.name
      out.printLine(" ")
      out.printLine(color("Total for specification" + (if (n.name.isEmpty) n.name.trim else " "+n.name.trim), blue, args.color))
      printStats(stats)
      out.printLine(" ")
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      val Stats(examples, successes, expectations, failures, errors, pending, skipped, timer, specStart, specEnd) = stats
      out.printLine(color("Finished in " + timer.time, blue, args.color))
      out.printLine(color(
          Seq(Some(examples qty "example"), 
              if (expectations != examples) Some(expectations qty "expectation") else None,
              Some(failures qty "failure"), 
              Some(errors qty "error"),
              pending optQty "pending", 
              skipped optInvariantQty "skipped").flatten.mkString(", "), blue, args.color))
    }
  }
  case class PrintOther(fragment: ExecutedFragment)   extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {}
  }
 
}
