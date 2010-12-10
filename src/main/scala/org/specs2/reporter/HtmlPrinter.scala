package org.specs2
package reporter

import scala.xml.NodeSeq
import scalaz.{ Monoid, Reducer, Scalaz, Generator, Foldable }
import Generator._
import data.Tuples._
import io._
import io.Paths._
import control.Throwablex._
import main.{ Arguments, SystemProperties }
import execute._
import specification._
import Statistics._
import Levels._
import SpecsArguments._

/**
 * The Html printer is used to create an Html report of an executed specification.
 * 
 * To do this, it uses a reducer to prepare print blocks with:
 * 
 * * the text to print
 * * the indentation level
 * * the statistics
 * * the current arguments to use
 *
 */
trait HtmlPrinter {
  /** the file system is used to copy resources and open the file to write */
  private[specs2] lazy val fileSystem = new FileSystem {}
  
  /** 
   * the output directory is either defined by a specs2 system variable
   * or chosen as a reports directory in the standard maven "target" directory
   */
  private[specs2] lazy val outputDir: String = 
    SystemProperties.getOrElse("outDir", "target/specs2-reports/").dirPath
  
  /**
   * print a sequence of executed fragments for a given specification class into a html 
   * file
   * the name of the html file is the full class name
   */
  def print(klass: Class[_], fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    copyResources
    
    fileSystem.write(reportPath(klass)) { out =>
      printHtml(new HtmlResultOutput(out), reduce(fs)).flush
    }
  }
  /** copy css and images file to the output directory */
  def copyResources = 
    Seq("css", "images").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
    
  /** @return the output file path for a given specification name*/
  def reportPath(klass: Class[_])(implicit args: Arguments) = {
    outputDir + klass.getName + ".html"
  }

  /** 
   * @return an HtmlResultOutput object containing all the html corresponding to the
   *         html lines to print  
   */  
  def printHtml(output: HtmlResultOutput, lines: HtmlLines)(implicit args: Arguments) = {
    output.enclose((t: NodeSeq) => <html>{t}</html>) {
      output.blank.printHead.enclose((t: NodeSeq) => <body>{t}</body>) {
        lines.printXml(output.blank)
      }
    }
  }
  
  /** @return the HtmlLines to print */  
  def reduce(fs: Seq[ExecutedFragment]) = 
    HtmlLines(flatten(FoldrGenerator[Seq].reduce(reducer, fs)))
  
  /** flatten the results of the reduction to a list of Html lines */
  private def flatten(results: (((List[Html], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[HtmlLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.toList zip levels.levels zip args.toList) map { 
      case (((t, s), l), a) => HtmlLine(t, s, l, a)
    }
  }  
  
  private  val reducer = 
    HtmlReducer &&& 
    StatisticsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer

  implicit object HtmlReducer extends Reducer[ExecutedFragment, List[Html]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_, _, _)  => HtmlSpecStart(start)
      case result @ ExecutedResult(_, _)    => HtmlResult(result)
      case text @ ExecutedText(s)              => HtmlText(text)
      case par @ ExecutedPar()                 => HtmlPar()
      case par @ ExecutedBr()                  => HtmlBr()
      case end @ ExecutedSpecEnd(_)            => HtmlSpecEnd(end)
      case fragment                            => HtmlOther(fragment)
    }
  }

}