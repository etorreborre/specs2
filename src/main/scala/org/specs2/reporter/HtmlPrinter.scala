package org.specs2
package reporter

import org.specs2.internal.scalaz.{Tree, TreeLoc, Reducer, Scalaz, Show}
import  Scalaz._
import collection.Iterablex._
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
import java.io.Writer

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
trait HtmlPrinter extends OutputDir {

  /**
   * print a sequence of executed fragments for a given specification class into a html 
   * file
   * the name of the html file is the full class name
   */
  def print(name: SpecName, fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    copyResources()
    val parentLink = HtmlLink(name, "", name.name)
    val htmlFiles = reduce(name, fs, parentLink)
    lazy val toc = globalToc(htmlFiles)
    htmlFiles.flatten.filter(_.nonEmpty).foreach { lines =>
      fileWriter.write(reportPath(lines.link.url)) { out =>
       write(printHtml(output, lines, globalTocDiv(toc, htmlFiles.rootLabel, lines)))(out)
      }
    }
  }

  /** @return a new HtmlReportOutput object creating html elements */
  def output: HtmlReportOutput = new HtmlResultOutput

  /** write the xml output to a Writer */
  def write(report: HtmlReportOutput)(out: Writer) = out.write(Xhtml.toXhtml(report.xml))

  /** @return a global toc */
  private def globalToc(htmlFiles: Tree[HtmlLines])(implicit args: Arguments) = {
    def itemsList(tree: Tree[HtmlLines]): NodeSeq = {
      val root = tree.rootLabel
      tocElements(root.printXml(output).xml, root.link.url, root.hashCode, { tree.subForest.map(itemsList).reduceNodes })
    }
    itemsList(htmlFiles)
  }
  /** @return a global toc to be displayed with jstree, focusing on the current section */
  private def globalTocDiv(toc: NodeSeq, root: HtmlLines, current: HtmlLines)(implicit args: Arguments) =
    <div id="tree">
      <ul>{toc}</ul>
      <script>{"""$(function () {	$('#tree').jstree({'core':{'initially_open':['"""+root.hashCode+"','"+current.hashCode+"""'], 'animation':200}, 'plugins':['themes', 'html_data']}); });"""}</script>
    </div>

  /** copy css and images file to the output directory */
  def copyResources() {
    Seq("css", "images", "css/themes/default").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
  }
    
  /**
   * @return an HtmlReportOutput object containing all the html corresponding to the
   *         html lines to print  
   */  
  def printHtml(output: =>HtmlReportOutput, lines: HtmlLines, globalToc: NodeSeq)(implicit args: Arguments) = {
    output printHtml (
		  output.printHead.
		         printBody(addToc(<div id="container">{lines.printXml(output).xml}</div>) ++ globalToc).xml
		)
	}

  /**
   * Organize the fragments into blocks of html lines to print, grouping all the fragments found after a link
   * into a single block that will be reported on a different html page
   *
   * This works by using a List of HtmlLines as a stack where the head of the list is the current block of lines
   *
   * @return the HtmlLines to print
   */
  def reduce(specification: SpecName, fs: Seq[ExecutedFragment], parentLink: HtmlLink): Tree[HtmlLines] = {
    val lines = flatten(fs.reduceWith(reducer))
    lazy val start: HtmlLines = HtmlLines(specification, Nil, parentLink)
    lines.foldLeft (leaf(start).loc) { (res, cur) =>
      val updated = res.updateLabel(_.add(cur))
      cur match {
        case HtmlLine(start @ HtmlSpecStart(s), st, l, a) if start.isIncludeLink =>
          updated.insertDownLast(leaf(HtmlLines(s.specName, List(HtmlLine(start.unlink, st, l, a)), link = start.link.getOrElse(parentLink))))
        case HtmlLine(HtmlSpecEnd(e, _), _, _, _) if e.specName == res.getLabel.specName => updated.getParent
        case other                                                                       => updated
      }
    }.root.tree
  }

  /** flatten the results of the reduction to a list of Html lines */
  private def flatten(results: (((List[Html], SpecStats), Levels[ExecutedFragment]), SpecsArguments[ExecutedFragment])): List[HtmlLine] = {
    val (prints, stats, levels, args) = results.flatten
    (prints zip stats.stats zip levels.levels zip args.nestedArguments) map {
      case (((t, s), l), a) => HtmlLine(t, s, l, a)
    }
  }  
  
  private  val reducer = 
    HtmlReducer &&& 
    StatsReducer &&&
    LevelsReducer  &&&
    SpecsArgumentsReducer

  implicit object HtmlReducer extends Reducer[ExecutedFragment, List[Html]] {
    implicit override def unit(fragment: ExecutedFragment) = List(print(fragment)) 
    /** print an ExecutedFragment and its associated statistics */
    def print(fragment: ExecutedFragment) = fragment match { 
      case start @ ExecutedSpecStart(_,_,_)       => HtmlSpecStart(start)
      case result @ ExecutedResult(_,_,_,_,_)     => HtmlResult(result)
      case text @ ExecutedText(s, _)              => HtmlText(text)
      case par @ ExecutedBr(_)                    => HtmlBr()
      case end @ ExecutedSpecEnd(_,_,s)           => HtmlSpecEnd(end, s)
      case fragment                               => HtmlOther(fragment)
    }
  }

}
