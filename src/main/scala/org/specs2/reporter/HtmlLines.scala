package org.specs2
package reporter

import text.Plural._
import text._
import main.Arguments
import execute._
import specification._

/**
 * The HtmlLines groups a list of HtmlLine to print
 * 
 * It can be written ('flushed') to an HtmlResultOuput by printing then one by one to this output
 *
 */
case class HtmlLines(lines : List[HtmlLine] = Nil, link: Option[HtmlLink] = None) {
  def print(implicit out: HtmlResultOutput, args: Arguments) =
    printXml.flush
  def printXml(implicit out: HtmlResultOutput) =
    lines.foldLeft(out) { (res, cur) => cur.print(res) }
  def add(line: HtmlLine) = HtmlLines(lines :+ line, link)
}

/** 
 * An HtmlLine encapsulates:
 * 
 * * an Html fragment to print
 * * the current statistics
 * * the current level
 * * the current arguments
 */
case class HtmlLine(text: Html = HtmlPar(), stats: (Stats, Stats) = (Stats(), Stats()), level: Int = 0, args: Arguments = Arguments()) {
  def print(implicit out: HtmlResultOutput): HtmlResultOutput = text.print(stats, level, args)
}

/**
 * This trait represents any kind of Html fragment to print
 * 
 * It has a print method taking an HtmlResultOutput and returning another HtmlResultOutput
 * containing the html code to print
 *
 */
sealed trait Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput): HtmlResultOutput
}
case class HtmlSpecStart(start: ExecutedSpecStart) extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    out.printSpecStart(start.name)(args)
}
case class HtmlText(t: ExecutedText) extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    out.printText(t.text, level, !args.xonly)(args)
}        
case class HtmlPar() extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    out.printElem(<p/>, !args.xonly)(args)
}
case class HtmlBr() extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    out.printPar("", !args.xonly)(args)
}
case class HtmlResult(r: ExecutedResult) extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    printResult(r.text, level, r.result)(args, out)
    
  def printResult(desc: MarkupString, level: Int, result: Result)(implicit args: Arguments, out: HtmlResultOutput): HtmlResultOutput = {
    result match {
      case f: Failure => printFailure(desc, level, f).printStack(f, level + 1, args.failtrace) 
      case e: Error   => printError(desc, level, e).printStack(e, level + 1) 
      case Success(_) => out.printSuccess(desc, level, !args.xonly)
      case Pending(_) => out.printPending(desc.append(" " + result.message), level, !args.xonly)
      case Skipped(_) => out.printSkipped(desc, level).printSkipped(NoMarkup(result.message), level, !args.xonly)
    }
  }
  def printFailure(desc: MarkupString, level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = {
    if (args.failtrace) 
      out.printFailure(desc, level).
          printCollapsibleExceptionMessage(f, level + 1)
    else
      out.printFailure(desc, level).
          printExceptionMessage(f, level + 1)
  }
  def printError(desc: MarkupString, level: Int, f: Result with ResultStackTrace)(implicit args: Arguments, out: HtmlResultOutput) = {
    out.printError(desc, level).
        printCollapsibleExceptionMessage(f, level + 1)
  }
}
case class HtmlSpecEnd(end: ExecutedSpecEnd) extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = {
    val (current, total) = stats
    if (total.isEnd(end)) printEndStats(total)(args, printCurrentStats(stats, args))
    else printCurrentStats(stats, args)
  }

  def printCurrentStats(stats: (Stats, Stats), args: Arguments)(implicit out: HtmlResultOutput) = {
    val (current, total) = stats
    if ((!args.xonly || current.hasFailuresOrErrors) && !total.isEnd(end)) printEndStats(current)(args, out)
    else out
  }
    
  def printEndStats(stats: Stats)(implicit args: Arguments, out: HtmlResultOutput) = {
    val name = end.name
    val title = "Total for specification" + (if (name.isEmpty) name.trim else " "+name.trim)
    val Stats(examples, successes, expectations, failures, errors, pending, skipped, specStart, specEnd) = stats
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
        <tr><td>Finished in</td><td class="info">{specStart.map(_.timer.time).getOrElse("0 second")}</td></tr>
        <tr><td>Results</td><td class={classStatus}>{numbers}</td></tr>
      </table>
    }
  }
}
case class HtmlSee(see: ExecutedSee) extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) =
    out.printStatusLink(see.link, level)(args)
}
case class HtmlOther(fragment: ExecutedFragment)   extends Html {
  def print(stats: (Stats, Stats), level: Int, args: Arguments)(implicit out: HtmlResultOutput) = out
}
