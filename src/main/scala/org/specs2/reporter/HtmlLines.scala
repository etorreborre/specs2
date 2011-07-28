package org.specs2
package reporter

import scala.xml._
import text.Plural._
import text._
import form._
import Form._
import main.Arguments
import execute._
import matcher.DataTable
import specification._
import org.specs2.internal.scalaz.Scalaz._
import Stats._

/**
 * The HtmlLines groups a list of HtmlLine to print
 * 
 * It can be written ('flushed') to an HtmlResultOuput by printing them one by one to this output
 *
 */
private[specs2]
case class HtmlLines(lines : List[HtmlLine] = Nil, link: HtmlLink) {
  def printXml(implicit out: HtmlResultOutput) = lines.foldLeft(out) { (res, cur) => cur.print(res) }
  def add(line: HtmlLine) = HtmlLines(lines :+ line, link)
  def is(name: SpecName) = link.is(name)
  def nonEmpty = !isEmpty
  def isEmpty = lines.isEmpty
  def updateSpecStartStats(s: Stats) = updateFirstLineStats { (stats: Stats) => s }
  def incrementSpecStartStats(s: Stats) = updateFirstLineStats { (stats: Stats) => stats |+| s }
  private def updateFirstLineStats(f: Stats => Stats) =
    copy(lines = lines.headOption.map { start => start.copy(stats = f(start.stats)) }.toList ++ lines.drop(1))
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
  def print(implicit out: HtmlResultOutput): HtmlResultOutput = text.print(stats, level, args)
}

/**
 * This trait represents any kind of Html fragment to print
 * 
 * It has a print method taking an HtmlResultOutput and returning another HtmlResultOutput
 * containing the html code to print
 *
 */
private[specs2]
sealed trait Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput): HtmlResultOutput
}
private[specs2]
case class HtmlSpecStart(start: ExecutedSpecStart) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    if (!args.xonly) out.printSpecStart(start.specName, stats)(args) else out
}
private[specs2]
case class HtmlText(t: ExecutedText) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    if (!args.xonly) out.printText(t.text, level, !args.xonly)(args) else out
}
private[specs2]
case class HtmlBr() extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    if (!args.xonly) out.printPar("", !args.xonly)(args) else out
}
private[specs2]
case class HtmlResult(r: ExecutedResult) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
    if (!args.xonly || !r.result.isSuccess) {
      r match {
        case ExecutedResult(FormMarkup(form), _, _, _) => printFormResult(form)(args, out)
        case _                                         => printResult(r.text(args), level, r.result)(args, out)
      }

    }
    else out
  }
  def printFormResult(form: Form)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = out.printElem(form.toXml(args))

  def printResult(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = {
    val outDesc = printDesc(desc, level, result)

    result match {
      case f: Failure                           => printFailureDetails(level + 1, f)(args, outDesc)
      case e: Error                             => printErrorDetails(level, e)(args, outDesc).printStack(e, level + 1)
      case Success(_)                           => outDesc
      case Skipped(_, _)                        => outDesc.printSkipped(NoMarkup(result.message), level, !args.xonly)
      case Pending(_)                           => outDesc.printPending(desc, level).printPending(NoMarkup(result.message), level, !args.xonly)
      case DecoratedResult(table: DataTable, r) => printDataTable(table, level)(args, outDesc)
    }
  }

  def printDesc(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput =
    result match {
      case f: Failure                           => out.printFailure(desc, level)
      case e: Error                             => out.printError(desc, level)
      case Success(_)                           => out.printSuccess(desc, level, !args.xonly)
      case Skipped(_, _)                        => out.printSkipped(desc, level).printSkipped(NoMarkup(result.message), level, !args.xonly)
      case Pending(_)                           => out.printPending(desc, level).printPending(NoMarkup(result.message), level, !args.xonly)
      case DecoratedResult(table: DataTable, r) => printDesc(desc, level, r)
    }

  def printFailureDetails(level: Int, f: Failure)(implicit args: Arguments, out: HtmlResultOutput) =
    if (args.failtrace) out.printCollapsibleExceptionMessage(f, level + 1).
                            printCollapsibleDetailedFailure(f.details, level + 1, args.diffs.show)
    else                out.printExceptionMessage(f, level + 1).
                            printCollapsibleDetailedFailure(f.details, level + 1, args.diffs.show)

  def printErrorDetails(level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) =
    out.printCollapsibleExceptionMessage(f, level + 1)

  def printDataTable(table: DataTable, level: Int = 0)(implicit args: Arguments, out: HtmlResultOutput) = printFormResult(Form(table))(args, out)

}
private[specs2]
case class HtmlSpecEnd(end: ExecutedSpecEnd) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
    if ((!args.xonly || stats.hasFailuresOrErrors) && stats.hasExpectations && stats.isEnd(end))
      printEndStats(stats)(args, out)
    else out
  }

  def printEndStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
    val title = "Total for specification" + (if (end.name.isEmpty) end.name.trim else " "+end.name.trim)
    val Stats(examples, successes, expectations, failures, errors, pending, skipped, timer, specStart, specEnd) = stats
    val classStatus = if (failures + errors > 0) "failure" else "success" 
    val numbers = Seq(
            Some(examples qty "example"), 
            if (expectations != examples) Some(expectations qty "expectation") else None,
            Some(failures qty "failure"), 
            Some(errors qty "error"),
            pending optQty "pending", 
            skipped optInvariantQty "skipped").flatten.mkString(", ")
            
    out.printBr().printElem {
      <table class="dataTable">
        <tr><th colSpan="2">{title}</th></tr>
        <tr><td>Finished in</td><td class="info">{timer.time}</td></tr>
        <tr><td>Results</td><td class={classStatus}>{numbers}</td></tr>
      </table>
    }
  }
}
private[specs2]
case class HtmlOther(fragment: ExecutedFragment)   extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = out
}
