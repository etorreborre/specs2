package org.specs2
package reporter

import io.{DirectoryPath, FilePath}
import main.Arguments
import data.Fold
import foldm._
import FoldM._
import stream._
import FoldProcessM._

import scalaz.concurrent.Task
import specification.core._
import specification.process._
import execute._
import text.NotNullStrings._

import scala.xml.NodeSeq
import matcher._
import form._
import org.specs2.time.SimpleTimer

/**
 * Create the body of an html file reporting a specification execution
 */
trait HtmlBodyPrinter {
  /**
   * Make the body of the Html file based on all the specification fragments
   */
  def makeBody(spec: SpecStructure, stats: Stats, timer: SimpleTimer, options: HtmlOptions, arguments: Arguments, pandoc: Boolean): String = {
    val title = spec.name
    type HtmlState = (String, Level)

    val htmlFold = fromFoldLeft[Fragment, HtmlState](("", Level())) { case ((html, level), fragment) =>
      (html + printFragment(arguments, level, options.outDir, pandoc)(fragment),
       Levels.fold(fragment, level))
    }

    val (html, _) = Fold.runFold(spec.fragments.contents, htmlFold.into[Task]).run
    html +
    s"""|
        |${printStatistics(title, stats, timer, options)}""".stripMargin
  }

  /**
   * Print a Fragment as a piece of Html
   *
   * If pandoc is true we make sure that text is not parsed in order to be correctly rendered as Markdown
   */
  def printFragment(arguments: Arguments, level: Level, baseDir: DirectoryPath, pandoc: Boolean) = (fragment: Fragment) => {

    fragment match {
      case t if Fragment.isText(t) =>
        val text = t.description.show
        if (text.trim.nonEmpty) {
          if (pandoc)
            scala.xml.Unparsed(text)
          else {
            // remove additional newlines and replace with just one when there is no markdown formatting
            val brStart = if (text.filterNot(_ == ' ').startsWith("\n")) <br/> else NodeSeq.Empty
            val brEnd   = if (text.filterNot(_ == ' ').endsWith("\n"))   <br/> else NodeSeq.Empty

            <text class="ok">{brStart}{scala.xml.Unparsed(text.trim)}{brEnd}</text>
          }
        } else NodeSeq.Empty

      case e if Fragment.isExample(e) =>
        val example =
          e.executionResult match {
            case r: Success =>
              <li class="example success ok">{show(e)}</li>

            case f1 @ Failure(m, e1, st, details) =>
              failureElement("example", f1, show(e), m, arguments.failtrace, details, arguments)

            case er @ Error(m, e1) =>
              errorElement("example", er, show(e), m, arguments)

            case r: Skipped =>
              <li class="example skipped ok">{show(e)}<br/>
                <message class="skipped">{r.message}</message>
              </li>

            case r: Pending =>
              <li class="example pending ok">{show(e)}<br/>
                <message class="pending">{r.message}</message>
              </li>

            // if it is a data table as an auto-example
            case DecoratedResult(table: DataTable, res) if Description.isCode(fragment.description) =>
              <div class={"example "+res.statusName(arguments)}>
                {Form(table).toXml(arguments)}<br/>
              </div>

            // if it is a failed data table
            case DecoratedResult(table: DataTable, res) if !res.isSuccess =>
              <li class={"example "+res.statusName(arguments)}>{show(e)}
                {Form(table).toXml(arguments)}<br/>
              </li>

            case DecoratedResult(table: DataTable, res) =>
              <li class="example success ok">{show(e)}</li>

            case r =>
              <li class="example info ok">{show(e)}<br/>
                <message class="info">{r.message}</message>
              </li>
          }

        if (level.incrementNext) <ul>{example}</ul>
        else                     example

      case f if Fragment.isStepOrAction(f) =>
        f.executionResult match {
          case f1 @ Failure(m, e1, st, details) =>
            failureElement("step", f1, <message class="failure">Failed step!</message>, m, arguments.failtrace, details, arguments)

          case er @ Error(m, e1) =>
            errorElement("step", er, <message class="error">Error in a step!</message>, m, arguments)

          case other => NodeSeq.Empty
        }

      case Fragment(ref: SpecificationRef,x,_) if !ref.hidden =>
        if (ref.muted) {
          <link class="ok"><a href={FilePath.unsafe(ref.url).relativeTo(baseDir).path} tooltip={ref.tooltip} class="ok">{ref.linkText}</a></link>
        } else {
          val status = fragment.executionResult.statusName(arguments)+" ok"
          val image = if (fragment.isExecutable) <span class={status}> </span> else NodeSeq.Empty
          <link class="ok">{image}  <a href={FilePath.unsafe(ref.url).relativeTo(baseDir).path} tooltip={ref.tooltip} class="ok">{ref.linkText}</a></link>
        }

      case Fragment(form @ FormDescription(_),_,_) =>
        form.xml(arguments)

      case Fragment(Br,_,_) => <br/>

      case other => NodeSeq.Empty
    }
  }

  def toggleElement(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  def id(a: Any) = System.identityHashCode(a).toString

  def show(f: Fragment) =
    f.description match {
      case Code(text) => <code class="prettyprint">{text}</code>
      case other =>
        val d = f.description.show
        <text>{if (Seq("*", "-").exists(d.trim.startsWith)) d.trim.drop(1) else d}</text>
    }


  def showStacktrace(id: String, st: List[StackTraceElement], klass: String, arguments: Arguments) =
    <stacktrace id={id} style="display:none" class={klass}>
      { arguments.traceFilter(st).map(t => <stacktrace-elt>{t.toString.replace("$", ".")}<br/></stacktrace-elt>).foldLeft(NodeSeq.Empty)(_ ++ _) }
    </stacktrace>

  def failureElement(element: String, f: Result with ResultStackTrace, description: Any, m: String, showTrace: Boolean, details: Details, arguments: Arguments) = {

    val message = <message class="failure">{m.notNull+" ("+f.location(arguments.traceFilter)+")"}</message>
    val detailedFailure = details match {
      case FailureDetails(expected, actual) if arguments.diffs.show(expected, actual) =>
        <details class="failure">"\nExpected:\n"{expected}"\nActual:\n"{actual}</details>

      case FailureSeqDetails(expected, actual) if arguments.diffs.showSeq(expected, actual, ordered = true) =>
        val (added, missing) = arguments.diffs.showSeqDiffs(actual, expected, ordered = true)
        val addedValues   = makeDifferencesMessage("Added", added)
        val missingValues = makeDifferencesMessage("Missing", missing)
        val details = List(addedValues, missingValues).mkString("\n")

        <details class="failure">{"\n" + added + "\n" + missing}</details>

      case FailureSetDetails(expected, actual) if arguments.diffs.showSeq(expected.toSeq, actual.toSeq, ordered = false) =>
        val (added, missing) = arguments.diffs.showSeqDiffs(actual.toSeq, expected.toSeq, ordered = true)
        val addedValues   = makeDifferencesMessage("Added", added.toSeq)
        val missingValues = makeDifferencesMessage("Missing", missing.toSeq)
        val details = List(addedValues, missingValues).mkString("\n")

        <details class="failure">{details}</details>

      case FailureMapDetails(expected, actual) if arguments.diffs.showMap(actual, expected)=>
        val (added, missing, different) = arguments.diffs.showMapDiffs(actual, expected)
        val addedValues   = makeDifferencesMessage("Added", added)
        val missingValues = makeDifferencesMessage("Missing", missing)
        val differentValues = makeDifferencesMessage("Different", different)
        val details = List(addedValues, missingValues, differentValues).mkString("\n")

        <details class="failure">{details}</details>

      case _ => NodeSeq.Empty
    }

    val fullMessage =
      if (showTrace) <li class ="failure toggle" onclick={toggleElement(f)}>{message}{detailedFailure}</li>
      else           <li class ="failure notoggle">{message}{detailedFailure}</li>

    val trace =
      if (showTrace) showStacktrace(id(f), f.stackTrace, "failure", arguments)
      else NodeSeq.Empty

    <li class={s"$element failure"}>{description}<br/>
      {fullMessage}
      {trace}</li>
  }

  def errorElement(element: String, er: Result with ResultStackTrace, description: Any, m: String, arguments: Arguments) = {
    <li class={s"$element error"}>{description}<br/>
      <li class ="error toggle" onclick={toggleElement(er)}>
        <message class="error">{m.notNull+" ("+er.location(arguments.traceFilter)+")"}</message>
      </li>
      {showStacktrace(id(er), er.stackTrace, "error", arguments)}
    </li>
  }

  /**
   * differences message
   */
  def makeDifferencesMessage(description: String, values: Seq[Any]): String =
    if (values.nonEmpty) s"\n$description (${values.size})  ${values.map(notNullPair).mkString("\n", "\n", "\n")}" else ""

  def printStatistics(title: String, stats: Stats, timer: SimpleTimer, options: HtmlOptions) =
    if (options.noStats) ""
    else {
      val statsClass = if (stats.hasErrors) "error" else if (stats.hasIssues) "failure" else "success"

      <table class="datatable">
        <tr><th colSpan="2">{s"Total for specification ${title.trim}"}</th></tr>
        <tr><td>Finished in</td><td class="info">{stats.copy(timer = timer).time}</td></tr>
        <tr><td>Results</td><td class={statsClass}>
          {stats.displayResults(Arguments("nocolor"))}</td></tr>
      </table>
    }
}

object HtmlBodyPrinter extends HtmlBodyPrinter
