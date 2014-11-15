package org.specs2
package reporter

import scalaz.{ Monoid, Reducer, Scalaz, Foldable, Applicative, State }
import control.Throwablex._
import Scalaz._
import Foldable._
import Monoid._
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
import execute.Error.ThrowableException
import reflect.ClassName._

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

  def print(name: SpecName, fs: Seq[ExecutingFragment])(implicit commandLineArgs: Arguments) = {
    fs.reduceWith(reducer)
  }

  private def reducer(implicit args: Arguments) =
    (PrintReducer            &&&
     StatsReducer2           &&&
     LevelsReducer2          &&&
     SpecsArgumentsReducer2) >>> printIO(textOutput)(args)

  type ToPrint = (((Stream[Print], Stats), Level[Fragment]), Arguments)

  /** print a line to the output */
  def printIO(output: ResultOutput)(implicit args: Arguments) = (line: ToPrint) => {
    line.flatten match {
      case (p, s, l, a) => PrintLine(p.last, s, l.level, args <| a).print(output)
    }
    line
  }

  case class PrintLine(text: Print, stats: Stats, level: Int, args: Arguments) {
    def print(implicit out: ResultOutput) = text.print(stats, level, args)
  }
  
  implicit val PrintReducer: Reducer[ExecutingFragment, Stream[Print]] = {
    /** print an ExecutedFragment and its associated statistics */
    def print: ExecutingFragment => Print = (fragment: ExecutingFragment) => fragment match {
      case PromisedExecutingFragment(_, _:Executable) => PrintResult(() => fragment.get)
      case other                                      => printExecuted(other.get)
    }

    def printExecuted: ExecutedFragment => Print = (fragment: ExecutedFragment) => fragment.get match {
      case start @ ExecutedSpecStart(_,_,_)    => PrintSpecStart(start)
      case result @ ExecutedResult(_,_,_,_,_)  => PrintResult(() => result)
      case text @ ExecutedText(s, _)           => PrintText(text)
      case par @ ExecutedBr(_)                 => PrintBr()
      case end @ ExecutedSpecEnd(_,_, s)       => PrintSpecEnd(end, s)
      case other                               => PrintOther(other)
    }
    Reducer.unitReducer { fragment: ExecutingFragment => Stream(print(fragment)) }
  }
    
  sealed trait Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput): Unit
    
    /**
     * indent the text to the wanted level.
     * If the text contains several lines, each line is indented
     */
    protected def leveledText(s: String, level: Int)(implicit args: Arguments): String = {
      val indent = "  "*level
      s.split("\n", -1).map(indent+_).mkString("\n")
    }
  }
  case class PrintSpecStart(start: ExecutedSpecStart) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      if (!start.hidden) {
        if (start.isSeeOnlyLink)         out.printSeeLink(leveledText(start.name, level)(args), stats)(args)
        else
          if (start.name != start.title) out.printSpecStartTitle(leveledText(start.title, level)(args), stats)(args)
          else                           out.printSpecStartName(leveledText(start.name, level)(args), stats)(args)
        if (args.xonly || args.hasFilter) out.printLine(" ")(args)
      }
    }
  }
  case class PrintResult(result: () => ExecutedFragment) extends Print {
    private lazy val executedResult = result() match {
      case e: ExecutedResult => Some(e)
      case other             => None
    }

    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      executedResult.map(r => printResult(if (r.flow) r.text(args).raw else leveledText(r.text(args).raw, level)(args), r.hasDescription, r.result, r.timer)(args, out))

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

              e.exception.chainedExceptions.foreach { t: Throwable =>
                out.printError("\nCaused by "+t.getClass.getName+": "+t.getMessage.notNull)
                args.traceFilter(t.getStackTrace.toSeq).foreach(st => out.printError(st.toString))
              }
            }
            case s @ Success(_,_)  => out.printSuccess(decoratedDescription(desc) + (if(!s.exp.isEmpty) "\n"+s.exp else ""))
            case Pending(_)        => out.printPending(decoratedDescription(desc) + " " + (if (result.message.isEmpty) "PENDING" else result.message))
            case Skipped(_, _) => {
              out.printText(decoratedDescription(desc))
              if (result.message != StandardResults.skipped.message) {
                if (result.message.isEmpty) out.printSkipped(" SKIPPED")
                else                        out.printSkipped(" "+result.message)
              }
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
      out.printText(description)
      val margin = desc.takeWhile(_ == ' ')+" "
      out.printFailure((if (isDataTable) f.message else
                                         f.message.split("\n").mkString(margin, "\n"+margin, "")) + location(f))
      if (args.failtrace)
        args.traceFilter(f.stackTrace).foreach(t => out.printFailure(t.toString))
    }

    def location(r: ResultStackTrace)(implicit args: Arguments) = " ("+r.location(args.traceFilter)+")" unless r.location.isEmpty

    def printFailureDetails(d: Details)(implicit args: Arguments, out: ResultOutput) = {
      d match {
        case FailureDetails(actual, expected) if args.diffs.show(actual, expected) =>
          val (actualDiff, expectedDiff) = args.diffs.showDiffs (actual, expected)
          printNewLine
          out.printFailure("Expected: " + expectedDiff)
          printNewLine
          out.printFailure("Actual:   " + actualDiff)
          if (args.diffs.showFull) {
            out.printFailure("Expected (full): " + expected)
            out.printFailure("Actual (full):   " + actual)
          }
          printNewLine

        case details @ FailureSeqDetails(actual, expected) if args.diffs.showSeq(actual, expected, ordered = true) =>
          val (added, missing) = args.diffs.showSeqDiffs(actual, expected, ordered = true)
          printNewLine
          printValues("Added", added); printNewLine
          printValues("Missing", missing); printNewLine
          printSummary(("Added", added), ("Missing", missing))

        case details @ FailureSetDetails(actual, expected) if args.diffs.showSeq(actual.toSeq, expected.toSeq, ordered = false) =>
          val (added, missing) = args.diffs.showSeqDiffs(actual.toSeq, expected.toSeq, ordered = false)
          printNewLine
          printValues("Added", added); printNewLine
          printValues("Missing", missing); printNewLine
          printSummary(("Added", added), ("Missing", missing))

        case details @ FailureMapDetails(actual, expected) if args.diffs.showMap(actual, expected) =>
          val (added, missing, different) = args.diffs.showMapDiffs(actual, expected)
          printNewLine
          printValues("Added", added); printNewLine
          printValues("Missing", missing); printNewLine
          printValues("Different", different); printNewLine
          printSummary(("Added", added), ("Missing", missing), ("Different", different))

        case _ => ()
      }
    }

    /** show values as a string with a description */
    def printValues(description: String, values: Seq[Any])(implicit args: Arguments, out: ResultOutput) =
      if (values.nonEmpty) out.printLines(s"$description (${values.size})${values.map(notNullPair).mkString("\n", "\n", "\n\n")}")

    /** print a short summary of differences between Seqs, Sets or Maps */
    def printSummary(descriptions: (String, Seq[String])*)(implicit args: Arguments, out: ResultOutput) =
      if (descriptions.flatMap(_._2).mkString("\n").split("\n").size >= 50)
        out.printLines(descriptions.map { case (name, values) => s"$name = ${values.size}" }.mkString(", "))

    /** print a newline */
    def printNewLine(implicit args: Arguments, out: ResultOutput) =
      out.printLine(" ")

    def printError(desc: String, f: Result with ResultStackTrace,
                   timer: SimpleTimer, isDataTable: Boolean = false)(implicit args: Arguments, out: ResultOutput) = {
      val description = statusAndDescription(desc, f, timer, isDataTable)(args, out)
      out.printText(description)
      val exceptionName = f.exception.getClass.simpleName
      val message = if (f.message.notNull == "null") "" else ": "+f.message
      val errorMessage =
        if (isDataTable) f.message + location(f)
        else             s"${desc.takeWhile(_ == ' ')} $exceptionName: $message ${location(f)}"

      out.printError(errorMessage)
    }
    /**
     * add the status to the description
     * making sure that the description is still properly aligned, even with several lines
     */
    def statusAndDescription(text: String, result: Result, timer: SimpleTimer, isDataTable: Boolean)(implicit args: Arguments, out: ResultOutput) = {
      val textLines = text.split("\n", -1)
      val firstLine = textLines.headOption.getOrElse("")
      val indentation = firstLine.takeWhile(_ == ' ').drop(2)
      def time = if (args.showtimes) " ("+timer.time+")" else ""

      val decoratedFirstLine = indentation + out.status(result)(args) + firstLine.dropWhile(_ == ' ') + time
      val rest = textLines.drop(1).map(l => if (isDataTable) s" $l" else s"  $l")
      (decoratedFirstLine +: rest).mkString("\n")
    }
  }

  case class PrintText(t: ExecutedText) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (args.canShow("-"))
        if (t.flow) out.printText(t.text)(args)
        else out.printText(leveledText(t.text, level)(args))(args)
  }        
  case class PrintBr() extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (args.canShow("-")) out.printText("\n")(args)
  }
  case class PrintSpecEnd(end: ExecutedSpecEnd, endStats: Stats) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {
      if (!end.isSeeOnlyLink && (args.xonly && stats.hasFailuresOrErrors || !args.xonly) && args.canShow("1"))
        printEndStats(stats)(args, out)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      out.printText("\n")
      val total = "Total for specification" + (if (end.title.isEmpty) end.title.trim else " "+end.title.trim)
      out.printStats(total)
      printStats(stats)
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      out.printStats(stats.display)
      out.printLine(" ")
    }
  }
  case class PrintOther(fragment: ExecutedFragment) extends Print {
    def print(stats: Stats, level: Int, args: Arguments)(implicit out: ResultOutput) = {}
  }
}
