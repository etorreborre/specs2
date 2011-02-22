package org.specs2
package reporter

import scala.xml.NodeSeq
import scalaz.{ Reducer, Scalaz, Generator }, Scalaz._
import html.TableOfContents._
import Generator._
import data.Tuples._
import io._
import io.Paths._
import main.{ Arguments, SystemProperties }
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
  /** the file system is used to open the file to write */
  private[specs2] lazy val fileSystem = new FileSystem {}
  /** the file writer is used to open the file to write */
  private[specs2] lazy val fileWriter = new FileWriter {}

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
  def print(s: SpecificationStructure, fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    copyResources()
    val parentLink = HtmlLink(SpecName(s), "", SpecName(s).name)
    reduce(fs, parentLink).foreach { lines =>
      fileWriter.write(reportPath(lines.link.url)) { out =>
        printHtml(new HtmlResultOutput(out), lines).flush
      }
    }
  }

  /** @return the file path for the html output */
  def reportPath(url: String) = outputDir + url

  /** copy css and images file to the output directory */
  def copyResources() =
    Seq("css", "images").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
    
  /**
   * @return an HtmlResultOutput object containing all the html corresponding to the
   *         html lines to print  
   */  
  def printHtml(output: HtmlResultOutput, lines: HtmlLines)(implicit args: Arguments) = {
    output.enclose((t: NodeSeq) => <html>{t}</html>) {
      output.blank.printHead.enclose((t: NodeSeq) => addToc(<body>{breadcrumbs(lines)}{t}</body>)) {
        lines.printXml(output.blank)
      }
    }
  }

  def breadcrumbs(lines: HtmlLines) = lines.breadcrumbs
  /**
   * Organize the fragments into blocks of html lines to print, grouping all the fragments found after a link
   * into a single block that will be reported on a different html page
   *
   * This works by using a List of HtmlLines as a stack where the head of the list is the current block of lines
   *
   * @return the HtmlLines to print
   */
  def reduce(fs: Seq[ExecutedFragment], parentLink: HtmlLink) = {
    flatten(FoldrGenerator[Seq].reduce(reducer, fs)).foldLeft (List(HtmlLines(Nil, parentLink, None))) { (res, cur) =>
      cur match {
        case HtmlLine(HtmlSee(see), _, _, _)          => HtmlLines(link = see.link, parent = Some(res.head)) :: (res.head.add(cur)) :: res.drop(1)
        case HtmlLine(HtmlSpecEnd(end), _, _, _)
          if (res.head.is(end.name))                  => res.drop(1) :+ res.head.add(cur)
        case other                                    => res.head.add(cur) :: res.drop(1)
      }
    }
  }

  /** flatten the results of the reduction to a list of Html lines */
  private def flatten(results: (((List[Html], SpecsStatistics), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[HtmlLine] = {
    val (prints, statistics, levels, args) = results.flatten
    (prints zip statistics.totals zip levels.levels zip args.toList) map {
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
      case start @ ExecutedSpecStart(_, _)     => HtmlSpecStart(start)
      case result @ ExecutedResult(_, _, _)    => HtmlResult(result)
      case text @ ExecutedText(s)              => HtmlText(text)
      case par @ ExecutedBr()                  => HtmlBr()
      case end @ ExecutedSpecEnd(_)            => HtmlSpecEnd(end)
      case see @ ExecutedSee(_)                => HtmlSee(see)
      case fragment                            => HtmlOther(fragment)
    }
  }

}