package org.specs2
package reporter

import scala.xml._
import text.Plural._
import text._
import form._
import main.Arguments
import execute._
import specification._

/**
 * The HtmlLines groups a list of HtmlLine to print
 * 
 * It can be written ('flushed') to an HtmlResultOuput by printing them one by one to this output
 *
 */
private[specs2]
case class HtmlLines(lines : List[HtmlLine] = Nil, link: HtmlLink, parent: Option[HtmlLines] = None) {
  def print(implicit out: HtmlResultOutput, args: Arguments) =
    printXml.flush
  def printXml(implicit out: HtmlResultOutput) =
    lines.foldLeft(out) { (res, cur) => cur.print(res) }
  def add(line: HtmlLine) = HtmlLines(lines :+ line, link, parent)
  def is(name: SpecName) = link.is(name)

  def breadcrumbs: NodeSeq = {
    if (parent.isDefined) <div id="breadcrumbs">{breadcrumbsLinks}</div>
    else NodeSeq.Empty
  }

  private def breadcrumbsLinks: NodeSeq = {
    val result = parent map { (p: HtmlLines) =>
      val separator = if (!p.breadcrumbsLinks.isEmpty) <t> / </t> else NodeSeq.Empty
      p.breadcrumbsLinks ++ separator ++ <a href={p.link.url}>{p.link.linkText}</a>
    }
    result.getOrElse(NodeSeq.Empty)
  }

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
    if (!args.xonly) out.printSpecStart(start.name)(args) else out
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
        case ExecutedResult(FormMarkup(form), _, _) => printFormResult(form)(args, out)
        case _                                      => printResult(r.text(args), level, r.result)(args, out)
      }

    }
    else out
  }
  def printFormResult(form: Form)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = out.printElem(form.toXml(args))

  def printResult(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = {
    result match {
      case f: Failure    => printFailure(desc, level, f).printStack(f, level + 1, args.failtrace)
      case e: Error      => printError(desc, level, e).printStack(e, level + 1)
      case Success(_)    => out.printSuccess(desc, level, !args.xonly)
      case Pending(_)    => out.printPending(desc, level).printPending(NoMarkup(result.message), level, !args.xonly)
      case Skipped(_, _) => out.printSkipped(desc, level).printSkipped(NoMarkup(result.message), level, !args.xonly)
    }
  }
  def printFailure(desc: MarkupString, level: Int, f: Failure)(implicit args: Arguments, out: HtmlResultOutput) = {
    if (args.failtrace) 
      out.printFailure(desc, level).
          printCollapsibleExceptionMessage(f, level + 1).
          printCollapsibleDetailedFailure(f.details, level + 1, args.diffs.show)
    else
      out.printFailure(desc, level).
          printExceptionMessage(f, level + 1).
          printCollapsibleDetailedFailure(f.details, level + 1, args.diffs.show)
  }
  def printError(desc: MarkupString, level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = {
    out.printError(desc, level).
        printCollapsibleExceptionMessage(f, level + 1)
  }
}
private[specs2]
case class HtmlSpecEnd(end: ExecutedSpecEnd) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
    if ((!args.xonly || stats.hasFailuresOrErrors) && stats.hasExpectations && stats.isEnd(end))
      printEndStats(stats)(args, out)
    else out
  }

  def printEndStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
    val n = end.name
    val title = "Total for specification" + (if (n.name.isEmpty) n.name.trim else " "+n.name.trim)
    val Stats(examples, successes, expectations, failures, errors, pending, skipped, timer, specStart, specEnd) = stats
    val classStatus = if (failures + errors > 0) "failure" else "success" 
    val numbers = Seq(
            Some(examples qty "example"), 
            if (expectations != examples) Some(expectations qty "expectation") else None,
            Some(failures qty "failure"), 
            Some(errors qty "error"),
            pending optQty "pending", 
            skipped optQty "skipped").flatten.mkString(", ")
            
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
case class HtmlSee(see: ExecutedSee) extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
    if (!args.xonly) out.printLink(see.link, level)(args) else out
  }
}
private[specs2]
case class HtmlOther(fragment: ExecutedFragment)   extends Html {
  def print(stats: Stats, level: Int, args: Arguments)(implicit out: HtmlResultOutput) = out
}
