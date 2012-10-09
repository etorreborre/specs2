package org.specs2
package reporter

import scala.xml._
import text._
import form._
import execute._
import matcher.DataTable
import specification._
import main.Arguments
import control.Identityx._
import html._
import xml.Nodex._
import io.Paths._

/**
* The HtmlFile class groups a list of HtmlLine objects to print to an output file for a given specification (identified by specName)
* and a html link for that file.
*
* It can be written ('flushed') to an HtmlResultOuput by printing the lines one by one to this output
*/
private[specs2]
case class HtmlLinesFile(specName: SpecName, args: Arguments,
                         link: HtmlLink, lines : Seq[HtmlLine] = Vector(), parent: Option[HtmlLinesFile] = None,
                         toc: TreeToc = TreeToc(SpecId(""))) {
  def print(out: =>HtmlReportOutput) = {
    def output = out.filePathIs(link.url)
    output.printHtml(
		  output.printHead.
		         printBody {
               breadcrumbs ++
               <div id="leftcolumn">{toc.toTree(specId)}</div> ++ <div id="central">{printLines(output).xml}</div> ++ <div id="rightcolumn"/>
             }.xml
      )
  }

  def printLines(out: HtmlReportOutput) = lines.foldLeft(out) { (res, cur) => cur.print(res) }
  
  def add(line: HtmlLine) = copy(lines = lines :+ line)
  def nonEmpty = !isEmpty
  def isEmpty = lines.isEmpty

  /** a unique identifier for the specification */
  def specId: SpecId = SpecId(specName.id.toString)

  /**
   * breadcrumbs are created but hidden by default
   * @return a breadcrumbs sequence for this file
   */
  def breadcrumbs: NodeSeq = <div id="breadcrumbs">{breadcrumbsLinks(link.url)}</div> unless !parent.isDefined

  /**
   * create breadcrumb links which will be embedded in a document having a specific url
   */
  def breadcrumbsLinks(relativeUrl: String): NodeSeq = {
    parent.map { (p: HtmlLinesFile) =>
      (if (p.parent.isDefined) p.breadcrumbsLinks(relativeUrl) else p.htmlLink(relativeUrl)) ++ <t> / </t> ++
      htmlLink(relativeUrl)
    }.getOrElse(NodeSeq.Empty)
  }

  /**
   * @return an anchor for the link corresponding to this file
   */
  def htmlLink(relativeUrl: String) = <a href={link.url.relativeTo(relativeUrl)}>{specName.name}</a>

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
  def stats: Stats
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
  def hidden        = start.hidden
  def unlink        = HtmlSpecStart(start.unlink)

  def print(out: HtmlReportOutput) = {
    out.when(!args.xonly) { output =>
      start.link.map(l => output.printLink(l, indent, stats, hidden)).getOrElse(output.printSpecStart(start.specName, stats))
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
	
  private def printFormResult(form: Form)(out: HtmlReportOutput): HtmlReportOutput = out.printForm(form.toXml(args))
  private def printFormResultWithIcon(form: Form, r: Result)(out: HtmlReportOutput): HtmlReportOutput = {
    val xml = form.toXml(args)
    r match {
      case f: Failure    => out.printFailureXml(xml, indent)
      case e: Error      => out.printErrorXml  (xml, indent)
      case Skipped(_, _) => out.printSkippedXml(xml, indent)
      case Pending(_)    => out.printPendingXml(xml, indent)
      case _             => out.printSuccessXml(xml, indent)
    }
  }

  private def printResult(desc: MarkupString, result: Result)(implicit out: HtmlReportOutput): HtmlReportOutput = {
    val outDesc = printDesc(desc, result)(out)
    implicit val doIt = !args.xonly
    result match {
      case f: Failure                                                    => printFailureDetails(f)(outDesc)
      case e: Error                                                      => printErrorDetails(e)(outDesc).printStack(e, indent + 1, args.traceFilter)
      case Success(_, _)                                                 => outDesc
      case Skipped(_, _)                                                 => outDesc ?> (_.printSkipped(NoMarkup(result.message), indent))
      case Pending(_)                                                    => outDesc ?> (_.printPending(NoMarkup(result.message), indent))
      case DecoratedResult(table: DataTable, r) if (desc.toHtml.isEmpty) => printDataTableExample(table, r)(out)
      case DecoratedResult(table: DataTable, r) if (r.isSuccess)         => outDesc
      case DecoratedResult(table: DataTable, r)                          => printDataTable(table)(outDesc)
      case DecoratedResult(other, r)                                     => printResult(desc, r)(out)
    }
  }

  def printDesc(desc: MarkupString, result: Result)(out: HtmlReportOutput): HtmlReportOutput = {
    result match {
      case f: Failure                           => out.printFailure(desc, indent)
      case e: Error                             => out.printError(desc, indent)
      case Success(_, _)                        => out.when(!args.xonly)(_.printSuccess(desc, indent))
      case Skipped(_, _)                        => out.printSkipped(desc, indent)
      case Pending(_)                           => out.printPending(desc, indent)
      case DecoratedResult(_, r)                => printDesc(desc, r)(out)
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
  /** print a DataTable without description */
  def printDataTableExample(table: DataTable, r: Result)(out: HtmlReportOutput) = printFormResultWithIcon(Form(table), r)(out)

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

