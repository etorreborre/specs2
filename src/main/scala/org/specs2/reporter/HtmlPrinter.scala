package org.specs2
package reporter

import scala.xml.NodeSeq
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
      val output = new HtmlResultOutput(out)
      output.enclose((t: NodeSeq) => <html>{t}</html>) {
        new HtmlResultOutput(out).printHead.enclose((t: NodeSeq) => <body>{t}</body>) {
          printElems(fs).printXml(new HtmlResultOutput(out))
        }
      }.flush
    }
  }
  def reportPath(klass: Class[_])(implicit args: Arguments) = {
    outputDir + klass.getName + ".html"
  }

  def printElems(fs: Seq[ExecutedFragment]) = 
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
    def print(implicit out: HtmlResultOutput) =
      printXml.flush
    def printXml(implicit out: HtmlResultOutput) =
      lines.foldLeft(out) { (res, cur) => cur.print(res) }
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
  }
  case class HtmlSpecStart(start: ExecutedSpecStart) extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      out.printSpecStart(start.name)(args)
  }
  case class HtmlText(t: ExecutedText)               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      out.printText(t.text, level, !args.xonly)(args)
  }        
  case class HtmlPar()                               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      out.printElem(<p/>, !args.xonly)(args)
  }
  case class HtmlBr()                               extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      out.printPar("", !args.xonly)(args)
  }
  case class HtmlResult(r: ExecutedResult)           extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
      printResult(r.text, level, r.result)(args, out)
      
    def printResult(desc: String, level: Int, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = {
      val description = desc
      result match {
        case f: Failure => printFailure(desc, level, f).printStack(f, level + 1, args.failtrace) 
        case e: Error   => printError(desc, level, e).printStack(e, level + 1) 
        case Success(_) => out.printSuccess(description, level, !args.xonly)
        case Pending(_) => out.printPending(description + " " + result.message, level, !args.xonly)
        case Skipped(_) => out.printSkipped(description, level).printSkipped(result.message, level, !args.xonly)
      }
    }
    def printFailure(desc: String, level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = { 
      out.printFailure(desc, level).
          printText("  " + f.message + " ("+f.location+")", level + 1)
    }
    def printError(desc: String, level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = { 
      out.printError(desc, level).
          printText(f.message + " ("+f.location+")", level + 1)
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
  case class HtmlSpecEnd(end: ExecutedSpecEnd)       extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
      val (current, total) = stats
      if (total.isEnd(end)) printEndStats(total)(args, printCurrentStats(stats, args))
      else printCurrentStats(stats, args)
    }

    def printCurrentStats(stats: (Stats, Stats), args: Arguments)(implicit out: HtmlResultOutput) = {
      val (current, total) = stats
      if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) printEndStats(current)(args, out)
      else out
    }
      
    def printEndStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
      val name = end.name
      val title = "Total for specification" + (if (name.isEmpty) name.trim else " "+name.trim)
      val Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart, specEnd) = stats
      val classStatus = if (failures + errors > 0) "failure" else "success" 
      val numbers = Seq(
              Some(examples qty "example"), 
              if (expectations != examples) Some(expectations qty "expectation") else None,
              Some(failures qty "failure"), 
              Some(errors qty "error"),
              pending optQty "pending", 
              skipped optQty "skipped").flatten.mkString(", ")
              
      out.printBr().printElem {
        <table class="dataTable">
          <tr><th colSpan="2">{title}</th></tr>
          <tr><td>Finished in</td><td class="info">{specStart.map(_.timer.time).getOrElse("0 second")}</td></tr>
          <tr><td>Results</td><td class={classStatus}>{numbers}</td></tr>
        </table>
      }
    }
  }
  case class HtmlOther(fragment: ExecutedFragment)   extends Html {
    def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = out
  }
  
    
}