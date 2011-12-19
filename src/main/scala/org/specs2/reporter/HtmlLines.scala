package org.specs2
package reporter

import scala.xml._
import text.Plural._
import text._
import form._
import Form._
import execute._
import matcher.DataTable
import specification._
import org.specs2.internal.scalaz.Scalaz._
import Stats._
import main.{Report, Arguments}
import control.Identityx._
import html.TableOfContents._

/**
* The HtmlFile class groups a list of HtmlLine objects to print to an output file for a given specification (identified by specName)
* and a html link for that file.
*
* It can be written ('flushed') to an HtmlResultOuput by printing the lines one by one to this output
*/
private[specs2]
case class HtmlLinesFile(specName: SpecName, link: HtmlLink, lines : Seq[HtmlLine] = Vector()) {
  def print(out: =>HtmlReportOutput, toc: NodeSeq) = {
    out.printHtml (
		  out.printHead.
		         printBody(addToc(<div id="container">{printLines(out).xml}</div>) ++ toc).xml
    )
  }

  def printLines(out: HtmlReportOutput) = lines.foldLeft(out) { (res, cur) => cur.print(res) }
  
  def add(line: HtmlLine) = copy(lines = lines :+ line)
  def nonEmpty = !isEmpty
  def isEmpty = lines.isEmpty
  
  override def toString = (link +: lines).mkString("\n")
}

/** 
 * An HtmlLine encapsulates:
 * 
 * - an executed fragment to print
 * - the current statistics
 * - the current level
 * - the current arguments
 */
private[specs2]
sealed trait HtmlLine {
  def level: Int
  def args: Arguments
  lazy val indent = if (args.noindent) 0 else level
  def print(out: HtmlReportOutput): HtmlReportOutput
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()): HtmlLine
}

private[specs2]
case class HtmlSpecStart(start: ExecutedSpecStart, stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def isSeeOnlyLink = start.isSeeOnlyLink
  def isIncludeLink = start.isIncludeLink
  def isLink        = start.isLink
  def link          = start.link
  def unlink        = HtmlSpecStart(start.unlink)

  def print(out: HtmlReportOutput) = {
    out.when(!args.xonly) { output =>
      start.link.map(l => output.printLink(l, indent, stats)).getOrElse(output.printSpecStart(start.specName, stats))
    } 
  }
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)

  override def toString = start.toString
}

private[specs2]
case class HtmlText(t: ExecutedText, stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def print(out: HtmlReportOutput) = out.when(!args.xonly)(_.printText(t.text, indent))
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)
  override def toString = t.toString
}

private[specs2]
case class HtmlBr(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def print(out: HtmlReportOutput) = out.when(!args.xonly)(_.printPar(""))
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)
}

private[specs2]
case class HtmlResult(r: ExecutedResult, stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def print(out: HtmlReportOutput) = {
    out.when(!args.xonly || !r.result.isSuccess) { output =>
      r match {
        case ExecutedResult(FormMarkup(form),_,_,_,_) => printFormResult(form)(output)
        case _                                        => printResult(r.text(args), r.result)(output)
      }
    }
  }
	
  def printFormResult(form: Form)(out: HtmlReportOutput): HtmlReportOutput = out.printForm(form.toXml(args))

  def printResult(desc: MarkupString, result: Result)(implicit out: HtmlReportOutput): HtmlReportOutput = {
    val outDesc = printDesc(desc, result)(out)
    implicit val doIt = !args.xonly
    result match {
      case f: Failure                           => printFailureDetails(f)(outDesc)
      case e: Error                             => printErrorDetails(e)(outDesc).printStack(e, indent + 1, args.traceFilter)
      case Success(_)                           => outDesc
      case Skipped(_, _)                        => outDesc ?> (_.printSkipped(NoMarkup(result.message), indent))
      case Pending(_)                           => outDesc ?> (_.printPending(NoMarkup(result.message), indent))
      case DecoratedResult(table: DataTable, r) => printDataTable(table)(outDesc)
    }
  }

  def printDesc(desc: MarkupString, result: Result)(out: HtmlReportOutput): HtmlReportOutput = {
    result match {
      case f: Failure                           => out.printFailure(desc, indent)
      case e: Error                             => out.printError(desc, indent)
      case Success(_)                           => out.when(!args.xonly)(_.printSuccess(desc, indent))
      case Skipped(_, _)                        => out.printSkipped(desc, indent)
      case Pending(_)                           => out.printPending(desc, indent)
      case DecoratedResult(table: DataTable, r) => printDesc(desc, r)(out)
    }
  }

  def printFailureDetails(f: Failure)(out: HtmlReportOutput) = {
    val outMessage =
      if (args.failtrace) out.printCollapsibleExceptionMessage(f, indent + 2)
      else                out.printExceptionMessage(f, indent + 2)

    outMessage.when(args.diffs.show)(_.printDetailedFailure(f.details, indent + 2, args.diffs))
  }

  def printErrorDetails(f: Result with ResultStackTrace)(out: HtmlReportOutput) =
    out.printCollapsibleExceptionMessage(f, indent + 1)

  def printDataTable(table: DataTable)(out: HtmlReportOutput) = printFormResult(Form(table))(out)
   
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)
  override def toString = r.toString
   
}

private[specs2]
case class HtmlSpecEnd(end: ExecutedSpecEnd, stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def print(out: HtmlReportOutput) = {
    implicit val doIt = (!args.xonly || stats.hasFailuresOrErrors) && stats.hasExpectations && (stats eq end.stats)
    implicit val arguments = args
    
    out ?> (_.printBr.printStats(end.specName, end.stats))
  }
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)
}

private[specs2]
case class HtmlOther(fragment: ExecutedFragment, stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) extends HtmlLine {
  def print(out: HtmlReportOutput) = out
  def set(stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) = copy(stats = stats, level = level, args = args)
}

