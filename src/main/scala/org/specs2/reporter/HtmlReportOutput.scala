package org.specs2
package reporter

import scala.xml.NodeSeq
import specification.{HtmlLink, Stats, SpecName}
import text.MarkupString
import execute.{Details, ResultStackTrace, Result}
import main.Diffs
import control.StackTraceFilter

/**
 * This trait defines how the specification elements should be translated as xhtml
 * An implementation of that trait must accumulate the results and return the created xhmtl
 * with the xml method
 */
trait HtmlReportOutput {
  /** @return the build html code */
	def xml: NodeSeq
  /** set the base directory */
  def baseDirIs(dir: String): HtmlReportOutput
  /** enclose the nodes inside <html/> tags */
	def printHtml(n: =>NodeSeq): HtmlReportOutput
  /** enclose the nodes inside <body/> tags */
  def printBody(n: =>NodeSeq): HtmlReportOutput
  /** provide the <head/> section */
  def printHead: HtmlReportOutput

  /** print a Br fragment */
	def printBr: HtmlReportOutput
  /** print a Text fragment inside a paragraph */
  def printPar(text: String = ""): HtmlReportOutput
  /** print a Text fragment with a given level of indentation */
  def printText(text: String = "", level: Int = 0): HtmlReportOutput
  /** print a Text fragment with a given level of indentation, in a paragraph */
  def printTextPar(text: String = "", level: Int = 0): HtmlReportOutput

  /** print the specification start fragment */
  def printSpecStart(name: SpecName, stats: Stats): HtmlReportOutput

  /** print a link to another specification */
  def printLink(link: HtmlLink, level: Int = 0, stats: Stats): HtmlReportOutput
  /** print some text with an icon */
  def printTextWithIcon(message: MarkupString, iconName: String, level: Int = 0): HtmlReportOutput
  /** print an issue with an icon */
  def printIssueWithIcon(message: MarkupString, iconName: String, level: Int = 0): HtmlReportOutput
  /** print a success with an icon */
  def printSuccess(message: MarkupString, level: Int = 0) = printTextWithIcon(message, "success",  level)
  /** print a failure with an icon */
  def printFailure(message: MarkupString, level: Int = 0) = printIssueWithIcon(message, "failure", level)
  /** print an error with an icon */
  def printError(message: MarkupString, level: Int = 0) = printIssueWithIcon(message, "error",   level)
  /** print a skipped fragment with an icon */
  def printSkipped(message: MarkupString, level: Int = 0) = printTextWithIcon(message, "skipped",  level)
  /** print a pending fragment with an icon */
  def printPending(message: MarkupString, level: Int = 0) = printTextWithIcon(message, "pending",  level)
  /** print an exception message */
  def printExceptionMessage(e: Result with ResultStackTrace, level: Int): HtmlReportOutput
  /** print an exception message which can be expended/collapsed */
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, level: Int): HtmlReportOutput
  /** print the details of a failure. The diffs arguments provides the way to expose the differences between expected and actual values */
  def printDetailedFailure(details: Details, level: Int, diffs: Diffs): HtmlReportOutput
  /** print a stacktrace of a failure or an exception */
  def printStack(e: ResultStackTrace, level: Int, traceFilter: StackTraceFilter): HtmlReportOutput

  /** print the html for a Form */
	def printForm(form: NodeSeq): HtmlReportOutput
  /** print the statistics of a specification */
  def printStats(name: SpecName, stats: Stats): HtmlReportOutput
}