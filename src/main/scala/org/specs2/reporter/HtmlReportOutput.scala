package org.specs2
package reporter

import scala.xml.{Elem, NodeSeq}
import specification.{HtmlLink, Stats, SpecName, FormattedString}
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
  /** show as a string */
  def show: String
  /** clear the current xml content */
  def clear: HtmlReportOutput
  /** print a NodeSeq */
  def print(xml: NodeSeq): HtmlReportOutput
  /** print an Elem */
  def print(xml: Elem): HtmlReportOutput
  /** print a Text Node */
  def print(text: String): HtmlReportOutput = print(scala.xml.Text(text))
  /** print a Text Node with a new line */
  def println(text: String = ""): HtmlReportOutput = print(text+"\n")
  /** set the file path for the current output */
  def filePathIs(path: String): HtmlReportOutput
  /** print the whole file content */
  def printFile(specName: SpecName, breadcrumbs: NodeSeq, lines: HtmlReportOutput, toc: TreeToc): HtmlReportOutput
  /** enclose the nodes inside <html/> tags */
  def printHtml(n: NodeSeq): HtmlReportOutput
  /** enclose the nodes inside <body/> tags */
  def printBody(n: NodeSeq): HtmlReportOutput
  /** provide the <head/> section */
  def printHead(title: String): HtmlReportOutput

  /** print a Br fragment */
  def printBr: HtmlReportOutput
  /** print a Text fragment inside a paragraph */
  def printPar(text: String = ""): HtmlReportOutput
  /** print a Text fragment with a given indent of indentation */
  def printText(text: FormattedString, indent: Int): HtmlReportOutput
  /** print a Text fragment with a given indent of indentation */
  def printText(text: String = "", indent: Int = 0): HtmlReportOutput
  /** print a Text fragment with a given indent of indentation, in a paragraph */
  def printTextPar(text: String = "", indent: Int = 0): HtmlReportOutput

  /** print the specification start fragment */
  def printSpecStart(name: SpecName, stats: Stats): HtmlReportOutput

  /** print a link to another specification */
  def printLink(link: HtmlLink, indent: Int, stats: Stats = Stats(), hidden: Boolean = false): HtmlReportOutput
  /** print a single link */
  def printLink(link: HtmlLink): HtmlReportOutput
  /** print some text with an icon */
  def printTextWithIcon(message: FormattedString, iconName: String, indent: Int = 0): HtmlReportOutput
  /** print an issue with an icon */
  def printIssueWithIcon(message: FormattedString, iconName: String, indent: Int = 0): HtmlReportOutput
  /** print some ok xml with an icon */
  def printOkXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0): HtmlReportOutput
  /** print some ok xml with an icon */
  def printKoXmlWithIcon(xml: NodeSeq, iconName: String, indent: Int = 0): HtmlReportOutput
  /** print a success with an icon */
  def printSuccess(message: FormattedString, indent: Int = 0) = printTextWithIcon(message, "success",  indent)
  /** print a failure with an icon */
  def printFailure(message: FormattedString, indent: Int = 0) = printIssueWithIcon(message, "failure", indent)
  /** print an error with an icon */
  def printError(message: FormattedString, indent: Int = 0) = printIssueWithIcon(message, "error",   indent)
  /** print a skipped fragment with an icon */
  def printSkipped(message: FormattedString, indent: Int = 0) = printTextWithIcon(message, "skipped",  indent)
  /** print a pending fragment with an icon */
  def printPending(message: FormattedString, indent: Int = 0) = printTextWithIcon(message, "pending",  indent)
  /** print a success with an icon */
  def printSuccessXml(xml: NodeSeq, indent: Int = 0) = printOkXmlWithIcon(xml, "success",  indent)
  /** print a failure with an icon */
  def printFailureXml(xml: NodeSeq, indent: Int = 0) = printKoXmlWithIcon(xml, "failure", indent)
  /** print an error with an icon */
  def printErrorXml(xml: NodeSeq, indent: Int = 0) = printKoXmlWithIcon(xml, "error",   indent)
  /** print a skipped fragment with an icon */
  def printSkippedXml(xml: NodeSeq, indent: Int = 0) = printOkXmlWithIcon(xml, "skipped",  indent)
  /** print a pending fragment with an icon */
  def printPendingXml(xml: NodeSeq, indent: Int = 0) = printOkXmlWithIcon(xml, "pending",  indent)
  /** print an exception message */
  def printExceptionMessage(e: Result with ResultStackTrace, indent: Int): HtmlReportOutput
  /** print an exception message which can be expended/collapsed */
  def printCollapsibleExceptionMessage(e: Result with ResultStackTrace, indent: Int): HtmlReportOutput
  /** print the details of a failure. The diffs arguments provides the way to expose the differences between expected and actual values */
  def printDetailedFailure(details: Details, indent: Int, diffs: Diffs): HtmlReportOutput
  /** print a stacktrace of a failure or an exception */
  def printStack(e: ResultStackTrace, indent: Int, traceFilter: StackTraceFilter): HtmlReportOutput

  /** print the html for a Form that succeeded */
  def printOkForm(form: NodeSeq): HtmlReportOutput
  /** print the html for a Form that failed */
  def printKoForm(form: NodeSeq): HtmlReportOutput
  /** print the statistics of a specification */
  def printStats(name: SpecName, stats: Stats): HtmlReportOutput
}