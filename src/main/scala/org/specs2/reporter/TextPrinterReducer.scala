package org.specs2
package reporter

import scalaz.{ Monoid, Reducer, Scalaz, Generator, Foldable }
import Generator._
import control.Throwablex._
import text.Plural._
import execute._
import main.Arguments
import specification._
import Statistics._
import LeveledBlocks._
import SpecsArguments._

trait TextPrinterReducer extends ResultOutput {
  def print(fs: Seq[ExecutedFragment]) = {
    PrintLines(flatten(FoldrGenerator[Seq].reduce(reducer, fs))).print
  }
  
  private  val reducer = ExecutedFragmentPrintReducer &&& 
    ExecutedFragmentsStatisticsReducer    &&&
    ExecutedFragmentLeveledBlocksReducer  &&&
    ExecutedFragmentSpecsArgumentsReducer
  
  case class PrintLine(text: Print = PrintPar(), stats: (Stats, Stats) = (Stats(), Stats()), level: Int = 0, args: Arguments = Arguments()) {
    def print = text.print(stats, level, args)
  }
  
  case class PrintLines(lines : List[PrintLine] = Nil) {
    def print = lines foreach (_.print)
  }
  
  def flatten(results: (((List[Print], SpecsStatistics), LeveledBlocks), SpecsArguments)): List[PrintLine] = {
    val prints = results._1._1._1
    val statistics = results._1._1._2.toList
    val levels = results._1._2.levels
    val args = results._2.toList
    (prints zip statistics zip levels zip args) map { 
      case (((t, s), l), a) => PrintLine(t, s, l, a)
    }
  }  
    
  implicit object ExecutedFragmentPrintReducer extends Reducer[ExecutedFragment, List[Print]] {
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
    def print(stats: (Stats, Stats), level: Int, args: Arguments): Unit 
    protected def leveledText(s: String, level: Int)(implicit args: Arguments): String = { 
      if (args.noindent) s 
      else (("  "*level) + s.trim)
    }
  }
  case class PrintSpecStart(start: ExecutedSpecStart) extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) = {
      printMessage(leveledText(start.name, level)(args))(args)
    } 
  }
  case class PrintResult(r: ExecutedResult)           extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) =
      printResult(leveledText(r.text, level)(args), r.result)(args)
      
    def printResult(desc: String, result: Result)(implicit args: Arguments): Unit = {
      val description = statusAndDescription(desc, result)(args)
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
    def statusAndDescription(s: String, result: Result)(implicit args: Arguments) = {
      (if (!args.plan) s.takeWhile(_ == ' ').dropRight(2) else s.takeWhile(_ == ' ')) + 
      status(result) + s.dropWhile(_ == ' ')
    }
    def status(result: Result)(implicit args: Arguments): String = {
      if (args.plan) ""
      else (result.status  + " ")
    }
  }
  case class PrintText(t: ExecutedText)               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) =
      if (!args.xonly) 
        printMessage(leveledText(t.text, level)(args))(args)
  }        
  case class PrintPar()                               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) =
      if (!args.xonly) printLine(" ")(args)
  }
  case class PrintBr()                               extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) =
      if (!args.xonly) printLine(" ")(args)
  }
  case class PrintSpecEnd(end: ExecutedSpecEnd)       extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) = {
      val (current, total) = stats
      if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) 
        printEndStats(current)(args)
      if (total.isEnd(end))
        printEndStats(total)(args)
    }
    def printEndStats(stats: Stats)(implicit args: Arguments) = {
      val name = end.name
      printLine(" ")
      printLine("Total for specification" + (if (name.isEmpty) name.trim else " "+name.trim))
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
  }
  case class PrintOther(fragment: ExecutedFragment)   extends Print {
    def print(stats: (Stats, Stats), level: Int, args: Arguments) = {}
  }
 
}
