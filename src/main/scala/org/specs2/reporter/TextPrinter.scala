package org.specs2
package reporter

import scalaz.{ Monoid, Reducer, Scalaz, Generator, Foldable }
import Generator._
import control.Throwablex._
import data.Tuples._
import text.Plural._
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
  
  def print(klass: Class[_], fs: Seq[ExecutedFragment])(implicit args: Arguments) = 
    printLines(fs).print(output)
  
  def printLines(fs: Seq[ExecutedFragment]) = 
    PrintLines(flatten(FoldrGenerator[Seq].reduce(reducer, fs)))
  
  private  val reducer = 
    PrintReducer &&& 
    StatisticsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer
  
  case class PrintLine(text: Print = PrintPar(), stats: (Stats, Stats) = (Stats(), Stats()), level: Int = 0, args: Arguments = Arguments()) {
    def print(implicit out: ResultOutput) = text.print(stats, level, args)
  }
  
  case class PrintLines(lines : List[PrintLine] = Nil) {
    def print(implicit out: ResultOutput) = lines foreach (_.print)
  }
  
  def flatten(results: (((List[Print], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[PrintLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.toList zip levels.levels zip args.toList) map { 
      case (((t, s), l), a) => PrintLine(t, s, l, a)
    }
  }  
    
  implicit object PrintReducer extends Reducer[ExecutedFragment, List[Print]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_, _, _)  => PrintSpecStart(start)
      case result @ ExecutedResult(_, _)       => PrintResult(result)
      case text @ ExecutedText(s)              => PrintText(text)
      case par @ ExecutedPar()                 => PrintPar()
      case par @ ExecutedBr()                  => PrintBr()
      case end @ ExecutedSpecEnd(_)            => PrintSpecEnd(end)
      case fragment                            => PrintOther(fragment)
    }
  }
    
  sealed trait Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput): Unit
    
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
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) = {
      out.printSpecStart(leveledText(start.name, level)(args))(args)
    } 
  }
  case class PrintResult(r: ExecutedResult)           extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) =
      printResult(leveledText(r.text, level)(args), r.result)(args, out)
      
    def printResult(desc: String, result: Result)(implicit args: Arguments, out: ResultOutput): Unit = {
      val description = statusAndDescription(desc, result)(args)
      result match {
        case f: Failure => {
          printFailureOrError(desc, f) 
          if (args.failtrace) 
            f.stackTrace.foreach(t => out.printError(t.toString))
        }
        case e: Error => {
          printFailureOrError(desc, e) 
          e.stackTrace.foreach(t => out.printError(t.toString))
          e.exception.chainedExceptions.foreach { (t: Throwable) =>
            out.printError(t.getMessage)
            t.getStackTrace.foreach(st => out.printError(st.toString))
          }
        }
        case Success(_) => if (!args.xonly) out.printSuccess(description)
        case Pending(_) => if (!args.xonly) out.printPending(description + " " + result.message)
        case Skipped(_) => if (!args.xonly) {
          out.printSkipped(description)
          out.printSkipped(result.message)
        }
      }
    }
    def printFailureOrError(desc: String, f: Result with ResultStackTrace)(implicit args: Arguments, out: ResultOutput) = { 
      val description = statusAndDescription(desc, f)
      out.printError(description)
      out.printError(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
    }
    /**
     * add the status to the description
     * making sure that the description is still properly aligned, even with several lines
     */
    def statusAndDescription(text: String, result: Result)(implicit args: Arguments) = {
      val textLines = text.split("\n") 
      val firstLine = textLines.take(1).map { s =>
        (if (!args.plan) s.takeWhile(_ == ' ').dropRight(2) else s.takeWhile(_ == ' ')) + 
        status(result) + s.dropWhile(_ == ' ')
      }
      val rest = textLines.drop(1)
      (firstLine ++ rest).mkString("\n")
    }
    def status(result: Result)(implicit args: Arguments): String = {
      if (args.plan) ""
      else (result.status  + " ")
    }
  }
  case class PrintText(t: ExecutedText)               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (!args.xonly) 
        out.printMessage(leveledText(t.text, level)(args))(args)
  }        
  case class PrintPar()                               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (!args.xonly) out.printLine(" ")(args)
  }
  case class PrintBr()                               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) =
      if (!args.xonly) out.printLine(" ")(args)
  }
  case class PrintSpecEnd(end: ExecutedSpecEnd)       extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) = {
      val (current, total) = stats
      if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) 
        printEndStats(current)(args, out)
      if (total.isEnd(end))
        printEndStats(total)(args, out)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      val name = end.name
      out.printLine(" ")
      out.printLine("Total for specification" + (if (name.isEmpty) name.trim else " "+name.trim))
      printStats(stats)
      out.printLine(" ")
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: ResultOutput) = {
      val Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart, specEnd) = stats
      stats.start.map(s => out.printLine("Finished in " + s.timer.time))
      out.printLine(
          Seq(Some(examples qty "example"), 
              if (expectations != examples) Some(expectations qty "expectation") else None,
              Some(failures qty "failure"), 
              Some(errors qty "error"),
              pending optQty "pending", 
              skipped optQty "skipped").flatten.mkString(", "))
    }
  }
  case class PrintOther(fragment: ExecutedFragment)   extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: ResultOutput) = {}
  }
 
}
