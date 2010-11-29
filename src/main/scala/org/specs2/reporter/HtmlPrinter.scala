package org.specs2
package reporter

import scalaz.{ Monoid, Reducer, Scalaz, Generator, Foldable }
import Generator._
import data.Tuples._
import text.Plural._
import io._
import io.Paths._
import control.Throwablex._
import main.{ Arguments, SystemProperties }
import execute._
import specification._
import Statistics._
import Levels._
import SpecsArguments._

trait HtmlPrinter {
  private[specs2] lazy val fileSystem = new FileSystem {}
  private[specs2] lazy val outputDir: String = 
    SystemProperties.getOrElse("outDir", "target/specs2-reports/").dirPath
  
  def print(klass: Class[_], fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    Seq("css", "images").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
    fileSystem.write(reportPath(klass)) { out => 
      printLines(fs).print(new HtmlResultOutput(out))
    }
  }
  def reportPath(klass: Class[_])(implicit args: Arguments) = {
    outputDir + klass.getName + ".html"
  }

  def printLines(fs: Seq[ExecutedFragment]) = 
    HtmlLines(flatten(FoldrGenerator[Seq].reduce(reducer, fs)))
  
  private  val reducer = 
    HtmlReducer &&& 
    StatisticsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer
  
  case class HtmlLine(text: Html = HtmlPar(), stats: (Stats, Stats) = (Stats(), Stats()), level: Int = 0, args: Arguments = Arguments()) {
    def print(implicit out: HtmlResultOutput): HtmlResultOutput = text.print(stats, level, args)
  }
  
  case class HtmlLines(lines : List[HtmlLine] = Nil) {
    def print(implicit out: HtmlResultOutput) = {
      lines.foldLeft(out) { (res, cur) => cur.print(res) }.flush
    }
  }
  
  def flatten(results: (((List[Html], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[HtmlLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.toList zip levels.levels zip args.toList) map { 
      case (((t, s), l), a) => HtmlLine(t, s, l, a)
    }
  }  
    
  implicit object HtmlReducer extends Reducer[ExecutedFragment, List[Html]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_, _, _)  => HtmlSpecStart(start)
      case result @ ExecutedResult(_, _)       => HtmlResult(result)
      case text @ ExecutedText(s)              => HtmlText(text)
      case par @ ExecutedPar()                 => HtmlPar()
      case par @ ExecutedBr()                  => HtmlBr()
      case end @ ExecutedSpecEnd(_)            => HtmlSpecEnd(end)
      case fragment                            => HtmlOther(fragment)
    }
  }
    
  sealed trait Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput): HtmlResultOutput
    
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
  case class HtmlSpecStart(start: ExecutedSpecStart) extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
      out.printSpecStart(leveledText(start.name, level)(args))(args)
    } 
  }
  case class HtmlResult(r: ExecutedResult)           extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      printResult(leveledText(r.text, level)(args), r.result)(args, out)
      
    def printResult(desc: String, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = {
      val description = statusAndDescription(desc, result)(args)
      result match {
        case f: Failure => {
          val out2 = printFailureOrError(desc, f) 
          if (args.failtrace) 
            f.stackTrace.foldLeft(out2) { (res, cur) => res.printError(cur.toString) }
          else out2
        }
        case e: Error => {
          val out2 = printFailureOrError(desc, e) 
          val out3 = e.stackTrace.foldLeft(out2) { (res, cur) => out.printError(cur.toString) }
          e.exception.chainedExceptions.foldLeft(out3) { (res, cur) =>
            val res2 = res.printError(cur.getMessage)
            cur.getStackTrace.foldLeft(res2) { (res3, st) => res3.printError(st.toString) }
          }
          
        }
        case Success(_) => if (!args.xonly) out.printSuccess(description) else out
        case Pending(_) => if (!args.xonly) out.printPending(description + " " + result.message) else out
        case Skipped(_) => if (!args.xonly) {
          out.printSkipped(description).printSkipped(result.message)
        } else out
      }
    }
    def printFailureOrError(desc: String, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = { 
      val description = statusAndDescription(desc, f)
      out.printError(description).
          printError(desc.takeWhile(_ == ' ') + "  " + f.message + " ("+f.location+")")
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
  case class HtmlText(t: ExecutedText)               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      if (!args.xonly) out.printPar(leveledText(t.text, level)(args))(args)
      else out
  }        
  case class HtmlPar()                               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      if (!args.xonly) out.printLine(" ")(args)
      else out
  }
  case class HtmlBr()                               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      if (!args.xonly) out.printLine(" ")(args)
      else out
  }
  case class HtmlSpecEnd(end: ExecutedSpecEnd)       extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
      val (current, total) = stats
      val out2 = if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) 
        printEndStats(current)(args, out)
      else out
      if (total.isEnd(end))
        printEndStats(total)(args, out2)
      else out2
    }
    def printEndStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
      val name = end.name
      val out2 = 
        out.printLine(" ").
            printLine("Total for specification" + (if (name.isEmpty) name.trim else " "+name.trim))
      printStats(stats)(args, out2).printLine(" ")
    }
    def printStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
      val Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart, specEnd) = stats
      val out2 = stats.start.foldLeft(out) { (res, s) => res.printLine("Finished in " + s.timer.time) }
      out2.printLine(
          Seq(Some(examples qty "example"), 
              if (expectations != examples) Some(expectations qty "expectation") else None,
              Some(failures qty "failure"), 
              Some(errors qty "error"),
              pending optQty "pending", 
              skipped optQty "skipped").flatten.mkString(", "))
    }
  }
  case class HtmlOther(fragment: ExecutedFragment)   extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = out
  }
  
    
}