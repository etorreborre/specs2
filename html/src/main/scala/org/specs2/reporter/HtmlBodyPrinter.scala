package org.specs2
package reporter

import io.{DirectoryPath, FilePath}
import main.Arguments
import specification.core.*
import specification.process.*
import execute.*
import text.NotNullStrings.*

import scala.xml.NodeSeq
import matcher.*
import fp.syntax.*
import form.*
import control.*
import origami.*, Folds.*
import control.producer.*
import concurrent.ExecutionEnv
import text.AnsiColors
import time.SimpleTimer

/** Create the body of an html file reporting a specification execution
  */
trait HtmlBodyPrinter:

  /** Make the body of the Html file based on all the specification fragments
    */
  def makeBody(
      spec: SpecStructure,
      stats: Stats,
      timer: SimpleTimer,
      options: HtmlOptions,
      arguments: Arguments,
      pandoc: Boolean
  )(ee: ExecutionEnv): Operation[String] =
    val title = spec.name
    type HtmlState = (String, Level)

    // Br (new line) fragment's description is meant for console output but
    // in html output examples are embedded in <li></li> tags and
    // there is no need to render additional blank line between them
    val deleteLineBetweenExamples: Transducer[Action, Fragment, Fragment] = producer =>
      producer.zipWithPreviousAndNext
        .filter {
          case (Some(f1), f2, Some(f3)) if Fragment.isExample(f1) && Fragment.isBr(f2) && Fragment.isExample(f3) =>
            false
          case _ => true
        }
        .map { case (_, f, _) =>
          f
        }

    val htmlFold = fromFoldLeft[Action, Fragment, HtmlState](("", Level())) { case ((htmlString, level), fragment) =>
      fragment.executionResult.map { result =>
        (
          htmlString + printFragment(fragment, result, arguments, level, options.outDir, pandoc),
          Levels.fold(fragment, level)
        )
      }
    }

    Operation.delayed(spec.fragments.contents.pipe(deleteLineBetweenExamples).fold(htmlFold).runOption(ee)).map {
      case Some((html, _)) =>
        html +
          s"""${printStatistics(title, stats, timer, options)}"""
      case _ =>
        s"<failed to produce the html for spec $spec>"
    }

  /** Print a Fragment as a piece of Html
    *
    * If pandoc is true we make sure that text is not parsed in order to be correctly rendered as Markdown
    */
  def printFragment(
      fragment: Fragment,
      result: Result,
      arguments: Arguments,
      level: Level,
      baseDir: DirectoryPath,
      pandoc: Boolean
  ): NodeSeq =
    fragment match
      case t if Fragment.isText(t) =>
        // remove Ansi colors in case some of them are present in the text
        val text = AnsiColors.removeColors(t.description.show)

        if text.trim.nonEmpty then
          if pandoc then scala.xml.Unparsed(text)
          else
            // remove additional newlines and replace with just one when there is no markdown formatting
            val brStart = if text.filterNot(_ == ' ').startsWith("\n") then <br/> else NodeSeq.Empty
            val brEnd = if text.filterNot(_ == ' ').endsWith("\n") then <br/> else NodeSeq.Empty

            <text class="ok">{brStart}{scala.xml.Unparsed(text.trim)}{brEnd}</text>
        else NodeSeq.Empty

      case Fragment(form @ FormDescription(_), _, _) =>
        form.xml(using arguments)

      case e if Fragment.isExample(e) =>
        val example =
          result match
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
            case r @ DecoratedResult(t, res) =>
              t.asInstanceOf[Matchable] match
                case table: DataTable if Description.isCode(fragment.description) =>
                  <div class={"example " + res.statusName(using arguments)}>
                    {Form(table).toXml(using arguments)}<br/>
                  </div>

                // if it is a failed data table
                case table: DataTable if !res.isSuccess =>
                  <li class={"example " + res.statusName(using arguments)}>{show(e)}
                    {Form(table).toXml(using arguments)}<br/>
                  </li>

                case table: DataTable =>
                  <li class="example success ok">{show(e)}</li>

                case _ =>
                  <li class="example info ok">{show(e)}<br/>
                    <message class="info">{r.message}</message>
                  </li>

        if level.incrementNext then <ul>{example}</ul>
        else example

      case f if Fragment.isStepOrAction(f) =>
        result match
          case f1 @ Failure(m, e1, st, details) =>
            failureElement(
              "step",
              f1,
              <message class="failure">Failed step!</message>,
              m,
              arguments.failtrace,
              details,
              arguments
            )

          case er @ Error(m, e1) =>
            errorElement("step", er, <message class="error">Error in a step!</message>, m, arguments)

          case other => NodeSeq.Empty

      case Fragment(ref: SpecificationRef, x, _) if !ref.hidden =>
        if ref.muted then
          <link class="ok"><a href={FilePath.unsafe(ref.url).relativeTo(baseDir).path} tooltip={
            ref.tooltip
          } class="ok">{ref.linkText}</a></link>
        
        else
          val status = result.statusName(using arguments) + " ok"
          val image = if fragment.isExecutable then <span class={status}> </span> else NodeSeq.Empty
          <link class="ok">{image}  <a href={FilePath.unsafe(ref.url).relativeTo(baseDir).path} tooltip={
            ref.tooltip
          } class="ok">{ref.linkText}</a></link>

      case Fragment(Br, _, _) =>
        <br/>

      case other =>
        NodeSeq.Empty

  def toggleElement(a: Any) = "toggleImage(this); showHide('" + id(a) + "')"
  def id(a: Any) = System.identityHashCode(a).toString

  def show(f: Fragment) =
    f.description match
      case Code(text) => <code class="prettyprint">{text}</code>
      case other =>
        val d = f.description.show
        <text>{if Seq("*", "-").exists(d.trim.startsWith) then d.trim.drop(1) else d}</text>

  def showStacktrace(id: String, st: List[StackTraceElement], klass: String, arguments: Arguments) =
    <stacktrace id={id} style="display:none" class={klass}>
      {arguments.traceFilter(st).map(t => <stacktrace-elt>{t.toString.replace("$", ".")}<br/></stacktrace-elt>).foldLeft(NodeSeq.Empty)(_ ++ _)}
    </stacktrace>

  def failureElement(
      element: String,
      f: Result & ResultStackTrace,
      description: Any,
      m: String,
      showTrace: Boolean,
      details: Details,
      arguments: Arguments
  ) =

    val message = <message class="failure">{m.notNull + " (" + f.location(arguments.traceFilter) + ")"}</message>
    val detailedFailure = details match
      case FailureDetails(expected, actual) if arguments.diffs.show(expected, actual) =>
        <details class="failure">"\nExpected:\n"{expected}"\nActual:\n"{actual}</details>

      case _ => NodeSeq.Empty

    val fullMessage =
      if showTrace then <li class ="failure toggle" onclick={toggleElement(f)}>{message}{detailedFailure}</li>
      else <li class ="failure notoggle">{message}{detailedFailure}</li>

    val trace =
      if showTrace then showStacktrace(id(f), f.stackTrace, "failure", arguments)
      else NodeSeq.Empty

    <li class={s"$element failure"}>{description}<br/>
      {fullMessage}
      {trace}</li>

  def errorElement(element: String, er: Result & ResultStackTrace, description: Any, m: String, arguments: Arguments) =
    <li class={s"$element error"}>{description}<br/>
      <li class ="error toggle" onclick={toggleElement(er)}>
        <message class="error">{m.notNull + " (" + er.location(arguments.traceFilter) + ")"}</message>
      </li>
      {showStacktrace(id(er), er.stackTrace, "error", arguments)}
    </li>

  /** differences message
    */
  def makeDifferencesMessage(description: String, values: Seq[Any]): String =
    if values.nonEmpty then s"\n$description (${values.size})  ${values.map(notNullPair).mkString("\n", "\n", "\n")}"
    else ""

  def printStatistics(title: String, stats: Stats, timer: SimpleTimer, options: HtmlOptions) =
    if options.noStats then ""
    else
      val statsClass = if stats.hasErrors then "error" else if stats.hasIssues then "failure" else "success"

      <table class="datatable">
        <tr><th colSpan="2">{s"Total for specification ${title.trim}"}</th></tr>
        <tr><td>Finished in</td><td class="info">{stats.copy(timer = timer).time}</td></tr>
        <tr><td>Results</td><td class={statsClass}>
          {stats.displayResults(using Arguments("nocolor"))}</td></tr>
      </table>

object HtmlBodyPrinter extends HtmlBodyPrinter
