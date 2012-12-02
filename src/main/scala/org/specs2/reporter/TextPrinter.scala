package org.specs2
package reporter

import org.specs2.internal.scalaz.{ Monoid, Reducer, Scalaz, Foldable, Applicative, State }
import control.Throwablex._
import Scalaz._
import data.Reducerx._
import collection.Seqx._
import data.Tuples._
import time._
import text._
import Trim._
import Plural._
import AnsiColors._
import NotNullStrings._
import execute._
import main.Arguments
import specification._
import Statistics._
import Levels._
import SpecsArguments._
import matcher.DataTable

/**
 * This trait reduces a list of ExecutedFragments to a list of PrintLines.
 * 
 * Each line contains:
 * - A description (text or example description)
 * - A level, to work out the indenting
 * - Some statistics, to print on SpecEnd
 * - The current arguments, to control the conditional printing of text, statistics,...
 *
 */
trait TextPrinter {
  def textOutput: ResultOutput = new TextResultOutput

  def print(name: SpecName, fs: Seq[ExecutedFragment])(implicit commandLineArgs: Arguments) = {
    fs.reduceWith(reducer)
  }

  private def reducer(implicit args: Arguments) =
    (PrintReducer           &&&
     StatisticsReducer      &&&
     LevelsReducer          &&&
     SpecsArgumentsReducer) >>> IOReducer(textOutput)(args)

  type ToPrint = (((Seq[Print], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])

  def IOReducer(output: ResultOutput)(implicit args: Arguments) =
    new Reducer[ToPrint, ToPrint] {
      override def unit(line: ToPrint) = {
        line.flatten match {
          case (p, s, l, a) => PrintLine(p.last, s.total, l.level, args <| a.last).print(output)
        }
        line
      }
    }

  case class PrintLine(text: Print, stats: Stats, level: Int, args: Arguments) {
    def print(implicit out: ResultOutput) = text.print(stats, level, args)
  }
  
  implicit object PrintReducer extends Reducer[ExecutedFragment, Seq[Print]] {
    implicit override def unit(fragment: ExecutedFragment) = Seq(print(fragment))
    /** print an ExecutedFragment and its associated statistics */
    def print: ExecutedFragment => Print = (fragment: ExecutedFragment) => fragment.get match {
      case start @ ExecutedSpecStart(_,_,_)    => PrintSpecStart(start)
      case result @ ExecutedResult(_,_,_,_,_)  => PrintResult(result)
      case text @ ExecutedText(s, _)           => PrintText(text)
      case par @ ExecutedBr(_)                 => PrintBr()
      case end @ ExecutedSpecEnd(_,_, s)       => PrintSpecEnd(end, s)
      case f                                   => PrintOther(f)
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
      if (!start.hidden) {
        if (start.name != start.title) out.printSpecStartTitle(leveledText(start.title, level)(args), stats)(args)
        else                           out.printSpecStartName(leveledText(start.name, level)(args), stats)(args)
        if (!args.xonly) out.printLine("")(args)
      }
    }
  }
  case class PrintResult(r: ExecutedResult)           extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      printResult(leveledText(r.text(args).toString, level)(args), r.hasDescription, r.result, r.timer)(args, out)
      
    def printResult(desc: String, hasDescription: Boolean, result: Result, timer: SimpleTimer)(implicit args: Arguments, out: ResultOutput): Unit = {
      def print(res: Result, desc: String, isDataTable: Boolean) {
        def decoratedDescription(d: String) = statusAndDescription(d, result, timer, isDataTable)(args, out)

        if (args.canShow(res.status)) {
          res match {
            case f @ Failure(m, e, st, d) => {
              printFailure(desc, f, timer, isDataTable)
              printFailureDetails(d)
            }
            case e: Error => {
              printError(desc, e, timer, isDataTable)
              args.traceFilter(e.stackTrace).foreach(t => out.printError(t.toString))
              e.exception.chainedExceptions.foreach { (t: Throwable) =>
                out.printError(t.getMessage.notNull)
                args.traceFilter(t.getStackTrace.toSeq).foreach(st => out.printError(st.toString))
              }
            }
            case s @ Success(_,_)  => out.printSuccess(decoratedDescription(desc) + (if(!s.exp.isEmpty) "\n"+s.exp else ""))
            case Pending(_)        => out.printPending(decoratedDescription(desc) + " " + result.message)
            case Skipped(_, _) => {
              out.printText(decoratedDescription(desc))
              if (!result.message.isEmpty)
                out.printSkipped(result.message)
            }
            case DecoratedResult(dt: DataTable, r) if !hasDescription && r.isSuccess       => print(r, dt.show, isDataTable = true)
            case DecoratedResult(dt: DataTable, r) if !hasDescription && !r.isSuccess      => print(r, "", isDataTable = true)
            case DecoratedResult(dt, r)                                                    => print(r, desc, isDataTable = true)
          }
        }
      }
      print(result, desc, false)
    }
    def printFailure(desc: String, f: Result with ResultStackTrace,
                     timer: SimpleTimer, isDataTable: Boolean = false)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer, isDataTable)(args, out)
      out.printFailure(description)
      val margin = desc.takeWhile(_ == ' ')+" "
      out.printFailure((if (isDataTable) f.message else
                                         f.message.split("\n").mkString(margin, "\n"+margin, "")) + location(f))
      if (args.failtrace)
        args.traceFilter(f.stackTrace).foreach(t => out.printFailure(t.toString))
    }

    def location(r: ResultStackTrace) = " ("+r.location+")" unless r.location.isEmpty

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
    def printError(desc: String, f: Result with ResultStackTrace,
                   timer: SimpleTimer, isDataTable: Boolean = false)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer, isDataTable)(args, out)
      out.printError(description)
      val exceptionName = f.exception.getClass.getSimpleName
      out.printError((if (isDataTable) "" else desc.takeWhile(_ == ' ')+"  "+exceptionName+": ") +
                     f.message + location(f))
    }
    /**
     * add the status to the description
     * making sure that the description is still properly aligned, even with several lines
     */
    def statusAndDescription(text: String, result: Result, timer: SimpleTimer, isDataTable: Boolean)(implicit args: Arguments, out: ResultOutput) = {
      val textLines = text.split("\n")
      val firstLine = textLines.headOption.getOrElse("")
      val indentation = firstLine.takeWhile(_ == ' ').dropRight(2)
      def time = if (args.showtimes) " ("+timer.time+")" else ""

      val decoratedFirstLine = indentation + out.status(result)(args) + firstLine.dropWhile(_ == ' ') + time
      val rest = textLines.drop(1).map(l => indentation + (if (isDataTable) "  " else "") + l)
      (decoratedFirstLine +: rest).mkString("\n")
    }
  }

  case class PrintText(t: ExecutedText)               extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (args.canShow("-"))
        out.printText(leveledText(t.text, level)(args))(args)
  }        
  case class PrintBr()                               extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (args.canShow("-")) out.printLine(" ")(args)
  }
  case class PrintSpecEnd(end: ExecutedSpecEnd, endStats: Stats)       extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      if ((args.xonly && stats.hasFailuresOrErrors || !args.xonly) && args.canShow("1"))
        printEndStats(stats)(args, out)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      out.printLine(" ")
      out.printStats("Total for specification" + (if (end.title.isEmpty) end.title.trim else " "+end.title.trim))
      printStats(stats)
      out.printLine("")
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      out.printLines(stats.display)
    }
  }
  case class PrintOther(fragment: ExecutedFragment)   extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {}
  }
}
