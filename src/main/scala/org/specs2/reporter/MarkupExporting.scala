package org.specs2
package reporter

import main.{Diffs, Arguments}
import specification._
import scala.xml.{Elem, Xhtml, NodeSeq}
import text.MarkupString
import execute.{Details, ResultStackTrace, Result}
import control.StackTraceFilter
import java.io.Writer

/**
 * Trait for exporting the specification as Markup files. It reuses most of the classes for writing html files but skips
 * the markup conversion for text
 */
trait MarkupExporting extends HtmlExporting {

  override def writeFiles(implicit args: Arguments = Arguments()) = (htmlFiles: Seq[HtmlFile]) => {
    htmlFiles.filter(_.nonEmpty).foreach(writeFile)
  }

  /** write the xml output to a Writer */
  override protected def writeXml(xml: NodeSeq)(out: Writer) {
    out.write(xml.text)
  }

  /** @return the file path for the markdown output */
  override def reportPath(url: String) = outputDir + url.replace(".html", markupExtension)

  /** markdown and .md is the default output format */
  def markupExtension = ".md"

  /** @return a new ReportOutput object creating markup elements */
  override def output: HtmlReportOutput = new MarkupResultOutput
}
object MarkupExporting extends MarkupExporting

/**
 * Implementation of a ReportOutput where most of the work is delegated to the HtmlResultOutput but where
 * the layout of html elements (html, body, head,...) is skipped
 * @param output
 */
private[specs2]
case class MarkupResultOutput(
  output: HtmlReportOutput = HtmlResultOutput(textPrinter = (t: String) => scala.xml.Text(t))) extends HtmlReportOutput {

  def xml: NodeSeq                                                                              = output.xml
  def print(xml: NodeSeq): MarkupResultOutput                                                   = MarkupResultOutput(output.print(xml))
  def print(xml: Elem)   : MarkupResultOutput                                                   = MarkupResultOutput(output.print(xml))
  def filePathIs(path: String)                                                                  = MarkupResultOutput(output.filePathIs(path))
  def printHtml(n: =>NodeSeq)                                                                   = print(n.map(_.text).mkString("\n"))
  def printBody(n: =>NodeSeq)                                                                   = print(n.map(_.text).mkString("\n"))
  def printHead                                                                                 = MarkupResultOutput(output)
  def printBr                                                                                   = println()
  def printPar(text: String = "")                                                               = println("\n"+text)
  def printText(text: String = "", level: Int = 0)                                              = MarkupResultOutput(output.printText(text, level))
  def printTextPar(text: String = "", level: Int = 0)                                           = print("\n").printText(text, level).print("\n")
  def printSpecStart(name: SpecName, stats: Stats)                                              = println("## "+name.title)
  def printIssueWithIcon(message: MarkupString, iconName: String, level: Int = 0)               = MarkupResultOutput(output.printIssueWithIcon(message, iconName, level))
  def printTextWithIcon(message: MarkupString, iconName: String, level: Int = 0)                = MarkupResultOutput(output.printTextWithIcon(message, iconName, level))
  def printExceptionMessage(e: Result with ResultStackTrace, level: Int)                        = MarkupResultOutput(output.printExceptionMessage(e, level))
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int)             = MarkupResultOutput(output.printCollapsibleExceptionMessage(e, level))
  def printDetailedFailure(details: Details, level: Int, diffs: Diffs)                          = MarkupResultOutput(output.printDetailedFailure(details, level, diffs))
  def printStack(e: ResultStackTrace, level: Int, traceFilter: StackTraceFilter)                = MarkupResultOutput(output.printStack(e, level, traceFilter))
  def printForm(form: NodeSeq)                                                                  = MarkupResultOutput(output.printForm(form))
  def printStats(name: SpecName, stats: Stats)                                                  = MarkupResultOutput(output.printStats(name, stats))
  def printLink(link: HtmlLink)                                                                 = MarkupResultOutput(output.printLink(link))
  def printLink(link: HtmlLink, level: Int = 0, stats: Stats = Stats(), hidden: Boolean = false)= {
    if (hidden) this
    else print(" * ").println(HtmlResultOutput().printLink(link).xml.toString)
  }
}
