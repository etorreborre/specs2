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
/**
 * The HtmlLines class groups a list of HtmlLine objects to print to an output file for a given specification (identified by specName)
 * and a html link for that file.
 * 
 * It can be written ('flushed') to an HtmlResultOuput by printing the lines one by one to this output
 */
private[specs2]
case class HtmlLines(specName: SpecName, lines : List[HtmlLine] = Nil, link: HtmlLink) {
  def printXml(implicit out: HtmlReportOutput) = lines.foldLeft(out) { (res, cur) => cur.print(res) }
  def add(line: HtmlLine) = copy(lines = lines :+ line)
  def nonEmpty = !isEmpty
  def isEmpty = lines.isEmpty
  
  override def toString = (link +: lines).mkString("\n")
}

/** 
 * An HtmlLine encapsulates:
 * 
 * * an Html fragment to print
 * * the current statistics
 * * the current level
 * * the current arguments
 */
private[specs2]
case class HtmlLine(text: Html = HtmlBr(), stats: Stats = Stats(), level: Int = 0, args: Arguments = Arguments()) {
  def print(implicit out: HtmlReportOutput): HtmlReportOutput = text.print(stats, if (args.noindent) 0 else level, args)
  
  override def toString = text.toString
}

/**
 * This trait represents any kind of Html fragment to print
 * 
 * It has a print method taking an HtmlReportOutput and returning another HtmlReportOutput
 * containing the html code to print
 *
 */
private[specs2]
sealed trait Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput): HtmlReportOutput
}

private[specs2]
case class HtmlSpecStart(start: ExecutedSpecStart) extends Html {
  def isSeeOnlyLink = start.isSeeOnlyLink
  def isIncludeLink = start.isIncludeLink
  def isLink        = start.isLink
  def link          = start.link
  def unlink        = HtmlSpecStart(start.unlink)

  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) = {
    out.when(!args.xonly) { output =>
      start.link.map(l => output.printLink(l, level, stats)).getOrElse(output.printSpecStart(start.specName, stats))
    } 
  }

  override def toString = start.toString
}

private[specs2]
case class HtmlText(t: ExecutedText) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) =
    out.when(!args.xonly)(_.printText(t.text, level))

  override def toString = t.toString
}

private[specs2]
case class HtmlBr() extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) =
    out.when(!args.xonly)(_.printPar(""))
}

private[specs2]
case class HtmlResult(r: ExecutedResult) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) = {
    out.when(!args.xonly || !r.result.isSuccess) { output =>
      r match {
        case ExecutedResult(FormMarkup(form),_,_,_,_) => printFormResult(form)(args, output)
        case _                                        => printResult(r.text(args), level, r.result)(args, output)
      }
    }
  }
	
  def printFormResult(form: Form)(implicit args: Arguments, out: HtmlReportOutput): HtmlReportOutput = out.printForm(form.toXml(args))

  def printResult(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlReportOutput): HtmlReportOutput = {
    val outDesc = printDesc(desc, level, result)
    implicit val doIt = !args.xonly
    result match {
      case f: Failure                           => printFailureDetails(level + 1, f)(args, outDesc)
      case e: Error                             => printErrorDetails(level, e)(args, outDesc).printStack(e, level + 1, args.traceFilter)
      case Success(_)                           => outDesc
      case Skipped(_, _)                        => outDesc ?> (_.printSkipped(NoMarkup(result.message), level))
      case Pending(_)                           => outDesc ?> (_.printPending(NoMarkup(result.message), level))
      case DecoratedResult(table: DataTable, r) => printDataTable(table, level)(args, outDesc)
    }
  }

  def printDesc(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlReportOutput): HtmlReportOutput = {
    result match {
      case f: Failure                           => out.printFailure(desc, level)
      case e: Error                             => out.printError(desc, level)
      case Success(_)                           => out.when(!args.xonly)(_.printSuccess(desc, level))
      case Skipped(_, _)                        => out.printSkipped(desc, level)
      case Pending(_)                           => out.printPending(desc, level)
      case DecoratedResult(table: DataTable, r) => printDesc(desc, level, r)
    }
  }

  def printFailureDetails(level: Int, f: Failure)(implicit args: Arguments, out: HtmlReportOutput) = {
    val outMessage =
      if (args.failtrace) out.printCollapsibleExceptionMessage(f, level + 1)
      else                out.printExceptionMessage(f, level + 1)

    outMessage.when(!args.diffs.show)(_.printCollapsibleDetailedFailure(f.details, level + 1, args.diffs))
  }

  def printErrorDetails(level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlReportOutput) =
    out.printCollapsibleExceptionMessage(f, level + 1)

  def printDataTable(table: DataTable, level: Int = 0)(implicit args: Arguments, out: HtmlReportOutput) = printFormResult(Form(table))(args, out)
   
  override def toString = r.toString
   
}

private[specs2]
case class HtmlSpecEnd(end: ExecutedSpecEnd, endStats: Stats) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) = {
    implicit val doIt = (!args.xonly || stats.hasFailuresOrErrors) && stats.hasExpectations && (stats eq endStats)
    implicit val arguments = args
    
    out ?> (_.printBr.printStats(title(end), stats))
  }

  private def title(end: ExecutedSpecEnd) = "Total for specification" + (if (end.name.isEmpty) end.name.trim else " "+end.name.trim)
}

private[specs2]
case class HtmlOther(fragment: ExecutedFragment)   extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlReportOutput) = out
}

