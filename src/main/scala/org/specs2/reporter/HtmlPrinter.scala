package org.specs2
package reporter

import org.specs2.internal.scalaz.{Tree, TreeLoc, Reducer, Scalaz, Generator, Show}
import  Scalaz._
import Generator._
import control.Exceptions._
import html._
import data.Trees._
import data.Tuples._
import xml.Nodex._
import TableOfContents._
import io._
import io.Paths._
import main.{ Arguments, SystemProperties }
import specification._
import Statistics._
import Levels._
import SpecsArguments._
import scala.xml.{Xhtml, NodeSeq}

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
  private[specs2] lazy val outputDir: String = SystemProperties.getOrElse("outDir", "target/specs2-reports/").dirPath
  
  /**
   * print a sequence of executed fragments for a given specification class into a html 
   * file
   * the name of the html file is the full class name
   */
  def print(s: SpecificationStructure, fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    copyResources()
    val parentLink = HtmlLink(s.content.start.name, "", s.content.start.name.name)
    val htmlFiles = reduce(fs, parentLink)
    lazy val toc = globalToc(htmlFiles)
    htmlFiles.flatten.filter(_.nonEmpty).foreach { lines =>
      fileWriter.write(reportPath(lines.link.url)) { out =>
        printHtml(new HtmlResultOutput, lines, globalTocDiv(toc, htmlFiles.rootLabel, lines)).flush(out)
      }
    }
  }

  /** @return a global toc */
  private def globalToc(htmlFiles: Tree[HtmlLines])(implicit args: Arguments) = {
    def itemsList(tree: Tree[HtmlLines]): NodeSeq = {
      val root = tree.rootLabel
      tocElements(root.printXml(new HtmlResultOutput).xml, root.link.url, root.hashCode, { tree.subForest.map(itemsList).reduceNodes })
    }
    itemsList(htmlFiles)
  }
  /** @return a global toc to be displayed with jstree, focusing on the current section */
  private def globalTocDiv(toc: NodeSeq, root: HtmlLines, current: HtmlLines)(implicit args: Arguments) =
    <div id="tree">
      <ul>{toc}</ul>
      <script>{"""$(function () {	$('#tree').jstree({'core':{'initially_open':['"""+root.hashCode+"','"+current.hashCode+"""'], 'animation':200}, 'plugins':['themes', 'html_data']}); });"""}</script>
    </div>

  /** @return the file path for the html output */
  def reportPath(url: String) = outputDir + url

  /** copy css and images file to the output directory */
  def copyResources() {
    Seq("css", "images", "css/themes/default").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
  }
    
  /**
   * @return an HtmlResultOutput object containing all the html corresponding to the
   *         html lines to print  
   */  
  def printHtml(output: HtmlResultOutput, lines: HtmlLines, globalToc: NodeSeq)(implicit args: Arguments) = {
    output.enclose((t: NodeSeq) => <html>{t}</html>) {
      output.blank.printHead.enclose((t: NodeSeq) => addToc(<body><div id="container">{t}</div>{globalToc}</body>)) {
        lines.printXml(output.blank)
      }
    }
  }

  /**
   * Organize the fragments into blocks of html lines to print, grouping all the fragments found after a link
   * into a single block that will be reported on a different html page
   *
   * This works by using a List of HtmlLines as a stack where the head of the list is the current block of lines
   *
   * @return the HtmlLines to print
   */
  def reduce(fs: Seq[ExecutedFragment], parentLink: HtmlLink): Tree[HtmlLines] = {
    lazy val start: HtmlLines = HtmlLines(Nil, parentLink)
    flatten(FoldrGenerator[Seq].reduce(reducer, fs)).foldLeft (leaf(start).loc) { (res, cur) =>
      val updated = res.updateLabel(_.add(cur))
      def updateSeeStats(node: TreeLoc[HtmlLines], s: Stats) = {
        val see = node.getLabel.lines.lastOption.map(_.copy(stats = s))
        node.updateLabel((u: HtmlLines) => u.copy(lines = u.lines.dropRight(1) ++ see.toList))
      }

      cur match {
        case HtmlLine(HtmlSee(see @ ExecutedSee(link, true, _)), _, _, _)   => {
          val stats = statsFromFile(link)
          val updatedSpecStart = updated.updateLabel(_.incrementSpecStartStats(stats))
          updateSeeStats(updatedSpecStart, stats)
        }
        case HtmlLine(HtmlSee(see @ ExecutedSee(_, false, _)), _, _, _)  => updated.insertDownLast(leaf(HtmlLines(link = see.link)))
        // when reaching a spec end:
        // * update the spec start with the stats
        // * go up a level and update the last html line with the statistics of the included spec
        // the last line should be a See fragment
        case HtmlLine(HtmlSpecEnd(end), s, _, _) if (res.getLabel.is(end.name)) => {
          val updatedParent = updated.updateLabel(_.updateSpecStartStats(s)).getParent
          updateSeeStats(updatedParent, s)
        }
        case other                                                              => updated
      }
    }.root.tree
  }

  /**
   * get the statistics from an already existing file
   */
  private def statsFromFile(link: HtmlLink): Stats = {
    val contents = tryOrElse(fileSystem.readFile(reportPath(link.url)))("")
    val lastStats = "<stats.*></stats>".r.findAllIn(contents).toSeq.lastOption
    val lastStatsXml = tryOrElse(lastStats.map(scala.xml.XML.loadString))(None)
    lastStatsXml.map(Stats.fromXml).getOrElse(Stats())
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
      case start @ ExecutedSpecStart(_, _, _)     => HtmlSpecStart(start)
      case result @ ExecutedResult(_, _, _, _)    => HtmlResult(result)
      case text @ ExecutedText(s, _)              => HtmlText(text)
      case par @ ExecutedBr(_)                    => HtmlBr()
      case end @ ExecutedSpecEnd(_, _)            => HtmlSpecEnd(end)
      case see @ ExecutedSee(_, _, _)             => HtmlSee(see)
      case fragment                               => HtmlOther(fragment)
    }
  }

}
