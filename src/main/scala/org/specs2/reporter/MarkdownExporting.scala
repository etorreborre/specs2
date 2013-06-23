package org.specs2
package reporter

import main.{Diffs, Arguments}
import specification._
import scala.xml.{Elem, Xhtml, NodeSeq}
import execute.{Details, ResultStackTrace, Result}
import control.StackTraceFilter
import java.io.Writer
import text.MarkdownOptions

/**
 * Trait for exporting the specification as Markup files. It reuses most of the classes for writing html files but skips
 * the markup conversion for text
 */
trait MarkdownExporting extends HtmlExporting {

  override def writeFiles(implicit args: Arguments = Arguments()) = (htmlFiles: Seq[HtmlFile]) => {
    htmlFiles.filter(_.nonEmpty).foreach(writeFile)
  }

  /** write the xml output to a Writer */
  override protected def writeXml(xml: NodeSeq)(out: Writer) {
    out.write(xml.text)
  }

  /** @return the file path for the markdown output */
  override def reportPath(url: String) = outputDir + url.replace(".html", markdownExtension)

  /** markdown and .md is the default output format */
  def markdownExtension = ".md"

  /** @return a new ReportOutput object creating markup elements */
  override def output(implicit args: Arguments): HtmlReportOutput = new MarkdownResultOutput(
    HtmlResultOutput(customTextPrinter = Some((t: String, options: MarkdownOptions) => scala.xml.Text(t))))
}
object MarkdownExporting extends MarkdownExporting

/**
 * Implementation of a ReportOutput where most of the work is delegated to the HtmlResultOutput but where
 * the layout of html elements (html, body, head,...) is skipped
 * @param output
 */
private[specs2]
case class MarkdownResultOutput(output: HtmlReportOutput)(implicit args: Arguments) extends HtmlReportOutput {
  def clear                                                                                     = MarkdownResultOutput(output.clear)
  def xml: NodeSeq                                                                              = output.xml
  def lines: NodeSeq                                                                            = output.xml // don't post-process as Markdown
  def print(xml: NodeSeq): MarkdownResultOutput                                                 = MarkdownResultOutput(output.print(xml))
  def print(xml: Elem)   : MarkdownResultOutput                                                 = MarkdownResultOutput(output.print(xml))
  def filePathIs(path: String)                                                                  = MarkdownResultOutput(output.filePathIs(path))
  def printHtml(n: =>NodeSeq)                                                                   = print(n.map(_.text).mkString("\n"))
  def printBody(n: =>NodeSeq)                                                                   = print(n.map(_.text).mkString("\n"))
  def printHead(title: String)                                                                  = MarkdownResultOutput(output)
  def printBr                                                                                   = println()
  def printPar(text: String = "")                                                               = println("\n"+text)
  def printText(text: String = "", indent: Int = 0)                                             = MarkdownResultOutput(output.printText(text, indent))
  def printText(text: FormattedString, indent: Int)                                             = MarkdownResultOutput(output.printText(text.raw, indent))
  def printTextPar(text: String = "", indent: Int = 0)                                          = print("\n").printText(text, indent).print("\n")
  def printSpecStart(name: SpecName, stats: Stats)                                              = println("## "+name.title)
  def printIssueWithIcon(message: FormattedString, iconName: String, indent: Int = 0)           = MarkdownResultOutput(output.printIssueWithIcon(message, iconName, indent))
  def printTextWithIcon(message: FormattedString, iconName: String, indent: Int = 0)            = MarkdownResultOutput(output.printTextWithIcon(message, iconName, indent))
  def printOkXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0)                       = MarkdownResultOutput(output.printOkXmlWithIcon(xml, iconName, indent))
  def printKoXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0)                       = MarkdownResultOutput(output.printKoXmlWithIcon(xml, iconName, indent))
  def printExceptionMessage(e: Result with ResultStackTrace, indent: Int)                       = MarkdownResultOutput(output.printExceptionMessage(e, indent))
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, indent: Int)            = MarkdownResultOutput(output.printCollapsibleExceptionMessage(e, indent))
  def printDetailedFailure(details: Details, indent: Int, diffs: Diffs)                         = MarkdownResultOutput(output.printDetailedFailure(details, indent, diffs))
  def printStack(e: ResultStackTrace, indent: Int, traceFilter: StackTraceFilter)               = MarkdownResultOutput(output.printStack(e, indent, traceFilter))
  def printOkForm(form: NodeSeq)                                                                = MarkdownResultOutput(output.printOkForm(form))
  def printKoForm(form: NodeSeq)                                                                = MarkdownResultOutput(output.printKoForm(form))
  def printStats(name: SpecName, stats: Stats)                                                  = MarkdownResultOutput(output.printStats(name, stats))
  def printLink(link: HtmlLink)                                                                 = MarkdownResultOutput(output.printLink(link))
  def printLink(link: HtmlLink, indent: Int, stats: Stats = Stats(), hidden: Boolean = false)   = {
    if (hidden) this
    else print(" * "+HtmlResultOutput().printLink(link).xml.toString)
  }
}
