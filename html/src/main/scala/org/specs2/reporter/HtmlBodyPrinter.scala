package org.specs2
package reporter

import io.{FilePath, DirectoryPath}
import main.Arguments
import org.specs2.specification.core._
import specification.process.Stats
import execute._
import text.NotNullStrings._

import scala.xml.NodeSeq

/**
 * Create the body of an html file reporting a specification execution
 */
trait HtmlBodyPrinter {
  /**
   * Make the body of the Html file based on all the specification fragments
   */
  def makeBody(spec: SpecStructure, stats: Stats, options: HtmlOptions, arguments: Arguments, pandoc: Boolean): String = {
    val title = spec.name

    s"""${spec.fragments.fragments.map(printFragment(arguments, options.outDir, pandoc)).mkString("")}""" ++
      s"""${printStatistics(title, stats, options)}"""
  }

  /**
   * Print a Fragment as a piece of Html
   *
   * If pandoc is true we make sure that text is not parsed in order to be correctly rendered as Markdown
   */
  def printFragment(arguments: Arguments, baseDir: DirectoryPath, pandoc: Boolean) = (fragment: Fragment) => {

    fragment match {
      case t if Fragment.isText(t) =>
        val text = t.description.show

        if (text.trim.nonEmpty) {
          if (pandoc) scala.xml.Unparsed(text)
          else {
            // remove additional newlines and replace with just one when there is no markdown formatting
            val brStart = if (text.filterNot(_ == ' ').startsWith("\n")) <br/> else NodeSeq.Empty
            val brEnd   = if (text.filterNot(_ == ' ').endsWith("\n"))   <br/> else NodeSeq.Empty

            <text class="ok">{brStart}{scala.xml.Unparsed(text.trim)}{brEnd}</text>
          }
        } else NodeSeq.Empty

      case e if Fragment.isExample(e) =>
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

          case r =>
            <li class="example info ok">{show(e)}<br/>
              <message class="info">{r.message}</message>
            </li>
        }

      case f if Fragment.isStepOrAction(f) =>
        f.executionResult match {
          case f1 @ Failure(m, e1, st, details) =>
            failureElement("step", f1, <message class="failure">Failed step!</message>, m, arguments.failtrace, details, arguments)

          case er @ Error(m, e1) =>
            errorElement("step", er, <message class="error">Error in a step!</message>, m, arguments)

          case other => NodeSeq.Empty
        }

      case Fragment(link: SpecificationLink,_,_) =>
        <link class="ok"><a href={FilePath.unsafe(link.url).rebaseTo(baseDir).path} tooltip={link.tooltip} class="ok">{link.linkText}</a></link>

      case Fragment(form @ FormDescription(_),_,_) =>
        form.xml(arguments)

      case other => NodeSeq.Empty
    }
  }

  def toggleElement(a: Any) = "toggleImage(this); showHide('"+id(a)+"')"
  def id(a: Any) = System.identityHashCode(a).toString

  def show(f: Fragment) =
    f.description match {
      case Code(text) => <code>{text}</code>
      case other      => <text>{f.description.show}</text>
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

      case FailureSeqDetails(expected, actual) if arguments.diffs.show(expected, actual, ordered = true) =>
        val (added, missing) = arguments.diffs.showDiffs(expected, actual, ordered = true)
        <details class="failure">{"\n" + added + "\n" + missing}</details>

      case FailureUnorderedSeqDetails(expected, actual) if arguments.diffs.show(expected, actual, ordered = false) =>
        val (added, missing) = arguments.diffs.showDiffs(expected, actual, ordered = false)
        <details class="failure">{"\n" + added + "\n" + missing}</details>

      case other => NodeSeq.Empty
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

  def printStatistics(title: String, stats: Stats, options: HtmlOptions) =
    if (options.noStats) ""
    else {
      val statsClass = if (stats.hasErrors) "error" else if (stats.hasIssues) "failure" else "success"

      <table class="datatable">
        <tr><th colSpan="2">{s"Total for specification ${title.trim}"}</th></tr>
        <tr><td>Finished in</td><td class="info">{stats.time}</td></tr>
        <tr><td>Results</td><td class={statsClass}>
          {stats.displayResults(Arguments("nocolor"))}</td></tr>
      </table>
    }
}

object HtmlBodyPrinter extends HtmlBodyPrinter