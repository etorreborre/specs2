package org.specs2
package reporter

import main.{Diffs, Arguments}
import specification._
import scala.xml.{Elem, Xhtml, NodeSeq}
import text.Trim._
import execute.{Details, ResultStackTrace, Result, Skipped, Success, Failure, Pending, Error}
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
  override def output(implicit args: Arguments): HtmlReportOutput = MarkdownResultOutput()
}
object MarkdownExporting extends MarkdownExporting

/**
 * Implementation of a ReportOutput where most of the work is delegated to the HtmlResultOutput but where
 * the layout of html elements (html, body, head,...) is skipped
 * @param output
 */
private[specs2]
case class MarkdownResultOutput(text: StringBuilder = new StringBuilder)(implicit args: Arguments) extends HtmlReportOutput {
  private def htmlOutput        = HtmlResultOutput(customTextPrinter = Some((t: String, options: MarkdownOptions) => scala.xml.Text(t)))
  private def append(t: String) = MarkdownResultOutput(text.append(t))

  def clear                                                                                     = MarkdownResultOutput()
  def xml: NodeSeq                                                                              = scala.xml.Text(text.toString)
  def show: String                                                                              = text.toString
  override def print(t: String): MarkdownResultOutput                                           = append(t)
  def print(xml: NodeSeq): MarkdownResultOutput                                                 = append(xml.toString)
  def print(xml: Elem)   : MarkdownResultOutput                                                 = append(xml.toString)
  def filePathIs(path: String)                                                                  = this
  def printFile(specName: SpecName, breadcrumbs: NodeSeq, lines: HtmlReportOutput, toc: TreeToc)= append(lines.show)
  def printHtml(n: NodeSeq)                                                                     = print(n.map(_.text).filter(_.nonEmpty).mkString("\n"))
  def printBody(n: NodeSeq)                                                                     = print(n.map(_.text).filter(_.nonEmpty).mkString("\n"))
  def printHead(title: String)                                                                  = this
  def printBr                                                                                   = println()
  def printPar(text: String = "")                                                               = println("\n"+text)
  def printText(t: String = "", indent: Int = 0)                                                = append(t.offset(indent))
  def printText(text: FormattedString, indent: Int)                                             = printText(text.raw, indent)
  def printTextPar(text: String = "", indent: Int = 0)                                          = print("\n").printText(text, indent).print("\n")
  def printSpecStart(name: SpecName, stats: Stats)                                              = println("## "+name.title)
  def printIssueWithIcon(message: FormattedString, iconName: String, indent: Int = 0)           = printText(message.raw, indent).append(status(iconName))
  def printTextWithIcon(message: FormattedString, iconName: String, indent: Int = 0)            = printText(message.raw, indent).append(status(iconName))
  def printOkXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0)                       = append(htmlOutput.printOkXmlWithIcon(xml, iconName, indent).xml.toString)
  def printKoXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0)                       = append(htmlOutput.printKoXmlWithIcon(xml, iconName, indent).xml.toString)
  def printExceptionMessage(e: Result with ResultStackTrace, indent: Int, filter: StackTraceFilter) = printText(e.message+s" (${e.location(filter)})", indent)
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, indent: Int)            = this
  def printDetailedFailure(details: Details, indent: Int, diffs: Diffs)                         = this
  def printStack(e: ResultStackTrace, indent: Int, traceFilter: StackTraceFilter)               = printText(e.stackTrace.mkString("\n"), indent)
  def printOkForm(form: NodeSeq)                                                                = this
  def printKoForm(form: NodeSeq)                                                                = this
  def printStats(name: SpecName, stats: Stats)                                                  = printText(
    s"""#| ${name.title} |
        #| Finished in ${stats.time} |
        #| ${stats.displayResults(Arguments("nocolor"))} |""".stripMargin('#'))

  def printLink(link: HtmlLink)                                                                 = append(htmlOutput.printLink(link).xml.toString)
  def printLink(link: HtmlLink, indent: Int, stats: Stats = Stats(), hidden: Boolean = false)   = {
    if (hidden) this
    else print(" * "+HtmlResultOutput().printLink(link).xml.toString)
  }

  protected def status(iconName: String) =
    Seq(Success(), Failure(), Pending(), Skipped(), Error()).find(_.statusName == iconName).map(r => s" (${r.status})").getOrElse("")
}
