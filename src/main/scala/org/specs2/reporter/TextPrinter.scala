package org.specs2
package reporter

import org.specs2.internal.scalaz.{ Monoid, Reducer, Scalaz, Foldable }
import control.Throwablex._
import collection.Iterablex._
import data.Tuples._
import time._
import text._
import Plural._
import AnsiColors._
import NotNullStrings._
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
  
  def print(s: SpecificationStructure, fs: Seq[ExecutedFragment])(implicit commandLineArgs: Arguments) =
    printLines(fs).print(output)
  
  def printLines(fs: Seq[ExecutedFragment])(implicit commandLineArgs: Arguments = Arguments()) =
    PrintLines(flatten(fs.reduceWith(reducer)))

  private val reducer = 
    PrintReducer &&& 
    StatsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer
  
  case class PrintLine(text: Print, stats: Stats, level: Int, args: Arguments) {
    def print(implicit out: ResultOutput) = text.print(stats, level, args)
  }
  
  case class PrintLines(lines : List[PrintLine] = Nil) {
    def print(implicit out: ResultOutput) = lines foreach (_.print)
  }
  
  def flatten(results: (((List[Print], SpecStats), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment]))(implicit commandLineArgs: Arguments = Arguments()): List[PrintLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.stats zip levels.levels zip args.toList) map {
      case (((t, s), l), a) => PrintLine(t, s, l, commandLineArgs <| a)
    }
  }  
    
  implicit object PrintReducer extends Reducer[ExecutedFragment, List[Print]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_,_,_)    => PrintSpecStart(start)
      case result @ ExecutedResult(_,_,_,_,_)  => PrintResult(result)
      case text @ ExecutedText(s, _)           => PrintText(text)
      case par @ ExecutedBr(_)                 => PrintBr()
      case end @ ExecutedSpecEnd(_,_, s)       => PrintSpecEnd(end, s)
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
        s.trim.split("\n").map(indent+_).mkString("\n")
      }
    }
  }
  case class PrintSpecStart(start: ExecutedSpecStart) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      out.printSpecStart(leveledText(start.name, level)(args), stats)(args)
    } 
  }
  case class PrintResult(r: ExecutedResult)           extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      printResult(leveledText(r.text(args).toString, level)(args), r.result, r.timer)(args, out)
      
    def printResult(desc: String, result: Result, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput): Unit = {
      val description = statusAndDescription(desc, result, timer)(args, out)
      def print(res: Result) {
        res match {
          case f @ Failure(m, e, st, d) => {
            printFailure(desc, f, timer)
            printFailureDetails(d)
          }
          case e: Error => {
            printError(desc, e, timer)
            args.traceFilter(e.stackTrace).foreach(t => out.printError(t.toString))
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
          case DecoratedResult(_, r) => print(r)
        }
      }
      print(result)
    }
    def printFailure(desc: String, f: Result with ResultStackTrace, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer)
      out.printFailure(description)
      out.printFailure(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
      if (args.failtrace)
        args.traceFilter(f.stackTrace).foreach(t => out.printFailure(t.toString))
    }
    def printFailureDetails(d: Details)(implicit args: Arguments, out: ResultOutput) = {
      d match {
        case FailureDetails(expected, actual) if (args.diffs.show(expected, actual)) => {
          val (expectedDiff, actualDiff) = args.diffs.showDiffs(expected, actual)
          out.printFailure("Expected: " + expectedDiff)
          out.printFailure("Actual:   " + actualDiff)
          if (args.diffs.showFull) {
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
      out.printError(desc.takeWhile(_ == ' ') + "  " + f.exception.getClass.getSimpleName + ": " + f.message + " ("+f.location+")")
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
  case class PrintSpecEnd(end: ExecutedSpecEnd, endStats: Stats)       extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      if (!(stats eq endStats) && !args.xonly && stats.hasFailuresOrErrors)
        printEndStats(stats)(args, out)
      if (stats eq endStats)
        printEndStats(stats)(args, out)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      out.printLine(" ")
      out.printStats("Total for specification" + (if (end.title.isEmpty) end.title.trim else " "+end.title.trim))
      printStats(stats)
      out.printLine(" ")
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {

      val Stats(examples, successes, expectations, failures, errors, pending, skipped, trend, timer) = stats
      out.printLine(args.colors.stats("Finished in " + timer.time, args.color))
      out.printLine(args.colors.stats(
          Seq(Some(examples qty "example"), 
              if (expectations != examples) Some(expectations qty "expectation") else None,
              Some(failures qty "failure"), 
              Some(errors qty "error"),
              pending optQty "pending", 
              skipped optInvariantQty "skipped").flatten.mkString(", "), args.color))
    }
  }
  case class PrintOther(fragment: ExecutedFragment)   extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {}
  }
 
}
