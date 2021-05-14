package org.specs2
package reporter

import matcher.DataTable
import control.*
import collection.Listx.*
import origami.*
import fp.*, syntax.{given, *}
import specification.core.*
import specification.process.*
import text.NotNullStrings.*
import text.Trim
import time.*
import Trim.*
import execute.*
import main.Arguments
import LogLine.*

/**
 * Prints the result of a specification execution to the console (using the line logger provided by the environment)
 *
 * At the end of the run the specification statistics are displayed as well.
 */
case class TextPrinter(env: Env) extends Printer {
  def prepare(specifications: List[SpecStructure]): Action[Unit]  = Action.unit
  def finalize(specifications: List[SpecStructure]): Action[Unit] = Action.unit

  def sink(spec: SpecStructure): AsyncSink[Fragment] = {
    // statistics and indentation
    type S = ((Stats, Int), SimpleTimer)

    val values: Fold[Action, Fragment, S] { type S = ((Stats, Int), SimpleTimer) } =
      Statistics.fold `zip` Indentation.fold.into[Action] `zip` SimpleTimer.timerFold.into[Action]

    lazy val logger = env.printerLogger
    lazy val args   = env.arguments <| spec.arguments

    lazy val sink: AsyncSink[(Fragment, S)] =
      Folds.fromStart(start(logger, spec.header, args)) *>
        linesLoggerSink(logger, spec.header, args).
          contraflatMap[(Fragment, S)](printFragment(args))

    (values `observeWithState` sink).mapFlatten(printFinalStats(spec, args, logger))
  }

  /** run a specification */
  def run(spec: SpecStructure): Unit =
    print(spec).runVoid(env.specs2ExecutionEnv)

  def linesLoggerSink(logger: PrinterLogger, header: SpecHeader, args: Arguments): AsyncSink[List[LogLine]] =
    Folds.fromSink[Action, List[LogLine]](lines =>
      Action.pure(lines.foreach(_.log(logger))))

  def start(logger: PrinterLogger, header: SpecHeader, args: Arguments): Action[PrinterLogger] =
    Action.pure(printHeader(args)(header).foreach(_.log(logger))).as(logger)

  def printFinalStats(spec: SpecStructure, args: Arguments, logger: PrinterLogger): (((Stats, Int), SimpleTimer)) => Action[Unit] = { case ((stats, _), timer) =>
    Action.pure(printStats(spec.header, args, stats, timer).foreach(_.log(logger))) >>
    Action.pure(logger.close())
  }

  def printHeader(args: Arguments): SpecHeader => List[LogLine] = { (header: SpecHeader) =>
    if args.canShow("#") then List(header.show.info)
    else Nil
  }

  def printStats(header: SpecHeader, args: Arguments, stats: Stats, timer: SimpleTimer): List[LogLine] =
    if (args.xonly && stats.hasFailuresOrErrors) || (!args.xonly && args.canShow("1"))   then {
      val title = if header.show.isEmpty then "" else " "+header.show.trim

      printNewLine ++
      printNewLine ++
      List(
        s"Total for specification$title\n".info,
        stats.copy(timer = timer).display(using args).info)
    }
    else Nil

  /** transform a stream of fragments into a stream of strings for printing */
  def printFragment(args: Arguments): ((Fragment, ((Stats, Int), SimpleTimer))) => Action[List[LogLine]] = {
    case (fragment, ((stats, indentation), _)) =>
      fragment.executedResult.map { case ExecutedResult(result, timer) =>
        fragment match {
          // only print steps and actions if there are issues
          case Fragment(NoText, e, l) if e.isExecutable && !result.isSuccess =>
            printExecutable(NoText, result, l, timer, args, indentation)

          case Fragment(d @ SpecificationRef(_, _, _, _, hidden, muted), e, l)  =>
            if !hidden then
              if e.isExecutable && !muted
                then printExecutable(d, result, l ,timer, args, indentation)
                else List(d.show.info)
            else Nil

          case Fragment(d, e, l) if e.isExecutable && d != NoText =>
            printExecutable(d, result, l, timer, args, indentation)

          case Fragment(Br, e, l) =>
            if args.canShow("-") then printNewLine
            else Nil

          case Fragment(Code(text), e, l) =>
            if args.canShow("-") then List(indentText(text, indentation, indentationSize(args)).info)
            else Nil

          case Fragment(d, e, l) =>
            if args.canShow("-") then List(indentText(d.show, indentation, indentationSize(args)).info)
            else Nil
        }
      }
  }

  /** print an executed fragment: example, step, action */
  def printExecutable(description: Description, result: Result, fragmentLocation: Location, timer: SimpleTimer, args: Arguments, indentation: Int): List[LogLine] =

    if args.canShow(result.status) then {
      val text = description.show
      val show = indentText(showTime(statusAndDescription(text, result)(args), timer, args), indentation, indentationSize(args))

      def printResult(desc: String, r: Result): List[LogLine] =
        r match {
          case err: execute.Error        => printError(desc, err, fragmentLocation, args)
          case failure: execute.Failure  => printFailure(desc, failure, fragmentLocation, args)
          case success: execute.Success  => printSuccess(desc, success, args)
          case pending: execute.Pending  => printPending(desc, pending, args)
          case skipped: execute.Skipped  => printSkipped(desc, skipped, args)
          case DecoratedResult(_, r)     => printResult(desc, r)
        }

      result match {
        // special case for SpecificationRefs
        case DecoratedResult(t, r) =>
          t.asInstanceOf[Matchable] match
            case s: Stats => printOther(show, r, args)
            case d: DataTable =>
              // display the full table if it is an auto-example
              if Description.isCode(description) then
                printResult(indentText(r.message, indentation, indentationSize(args)), r.setMessage(""))
                else
                printResult(show, r)

        case other => printResult(show, result)
      }
    }
    else Nil

  def printError(show: String, result: execute.Error, fragmentLocation: Location, args: Arguments): List[LogLine] =
    List((show+"\n").error) ++
    printMessage(show, result, fragmentLocation, ErrorLine.apply, args) ++
    printStacktrace(result.stackTrace, print = true, ErrorLine.apply, args) ++
    (if result.exception.getCause != null then printError("CAUSED BY", execute.Error(result.exception.getCause), fragmentLocation, args)
     else List())

  def printFailure(show: String, failure: execute.Failure, fragmentLocation: Location, args: Arguments): List[LogLine] =
    (if args.xonly then List("\n".info) else List()) ++
    List((show+"\n").failure) ++
    printMessage(show, failure, fragmentLocation, FailureLine.apply, args) ++
    printStacktrace(failure.stackTrace, print = args.failtrace, FailureLine.apply, args) ++
    printFailureDetails(args)(failure.details)

  def printSuccess(show: String, success: execute.Success, args: Arguments): List[LogLine] = {
    val expected = if success.exp.nonEmpty then "\n"+success.exp else ""
    if expected.trim.nonEmpty
      then List((show+expected).info)
      else List(show.info)
  }

  def printPending(show: String, pending: execute.Pending, args: Arguments): List[LogLine] = {
    val reason = if pending.message.isEmpty then "PENDING" else pending.message

    if reason.trim.nonEmpty
      then List((show+" "+reason).info)
      else List((show).info)
  }

  def printSkipped(show: String, skipped: execute.Skipped, args: Arguments): List[LogLine] = {
    val reason =
      if skipped.message != StandardResults.skipped.message then
        if skipped.message.isEmpty then "SKIPPED" else skipped.message
      else skipped.message

    if reason.trim.nonEmpty
      then List((show+"\n"+reason).info)
      else List(show.info)
  }

  def printOther(show: String, other: execute.Result, args: Arguments): List[LogLine] =
    List(show.info)

  /** show execution times if the showtimes argument is true */
  def showTime(description: String, timer: SimpleTimer, args: Arguments) = {
    val time = if args.showtimes then " ("+timer.time+")" else ""
    description + time
  }

  def statusAndDescription(text: String, result: Result)(args: Arguments) = {
    val textLines = text.trimEnclosing("```").trimEnclosing("`").split("\n", -1).toList // trim markdown code marking
    val firstLine = textLines.headOption.getOrElse("")
    val (indentation, line) = firstLine.span(_ == ' ')
    val status = result.coloredStatus(using args) + " "
    val decoratedFirstLine = indentation + status + (if Seq("*", "-").exists(line.startsWith) then line.drop(2) else line)
    val rest = textLines.drop(1).map(l => s"  $l")
    (decoratedFirstLine +: rest).mkString("\n")
  }

  def indentationSize(args: Arguments): Int =
    args.commandLine.int("indentation").getOrElse(2)

  def printMessage(description: String, result: Result & ResultStackTrace, fragmentLocation: Location, as: String => LogLine, args: Arguments): List[LogLine] =
    val margin = description.takeWhile(_ == ' ')+" "
    List(as(result.message.split("\n").mkString(margin, "\n"+margin, " ") + location(result, fragmentLocation, args)))

  def printStacktrace(stacktrace: List[StackTraceElement], print: Boolean, as: String => LogLine, args: Arguments): List[LogLine] =
    if print
      then args.traceFilter(stacktrace).map(t => as(t.toString+"\n")).toList
      else Nil

  /**
   * If the failure contains the expected and actual values, display them
   */
  def printFailureDetails(args: Arguments): Details => List[LogLine] = {
    case FailureDetailsMessages(messages) =>
      messages.map(_.failure).intersperse("\n".failure)

    case FailureDetails(actual, expected) if args.diffs.show(actual, expected) =>
      val (actualDiff, expectedDiff) = args.diffs.showDiffs(actual, expected)
      val shortDiff =
        if actualDiff != expectedDiff then
          List(s"Actual:   $actualDiff\n".failure,
               s"Expected: $expectedDiff\n".failure)
        else List()

      val fullDiff =
        (if args.diffs.showFull then
          List(s"Actual (full):   $actual\n".failure,
               s"Expected (full): $expected\n".failure)
        else Nil)

      List("\n".failure) ++
      shortDiff ++
      List("\n".failure) ++
      fullDiff ++
      List("\n".info)

    case details @ FailureSeqDetails(actual, expected) if args.diffs.showSeq(actual, expected, ordered = true) =>
      val (added, missing) = args.diffs.showSeqDiffs(actual, expected, ordered = true)

      printNewLine ++
      printValues("Added", added) ++ printNewLine ++
      printValues("Missing", missing) ++ printNewLine ++
      printSummary(("Added", added), ("Missing", missing))


    case details @ FailureSetDetails(actual, expected) if args.diffs.showSeq(actual.toSeq, expected.toSeq, ordered = false) =>
      val (added, missing) = args.diffs.showSeqDiffs(actual.toSeq, expected.toSeq, ordered = false)
      printNewLine ++
      printValues("Added", added) ++ printNewLine ++
      printValues("Missing", missing) ++ printNewLine ++
      printSummary(("Added", added), ("Missing", missing))

    case details @ FailureMapDetails(actual, expected) if args.diffs.showMap(actual, expected) =>
      val (added, missing, different) = args.diffs.showMapDiffs(actual, expected)
      printNewLine ++
      printValues("Added", added) ++ printNewLine ++
      printValues("Missing", missing); printNewLine ++
      printValues("Different", different) ++ printNewLine ++
      printSummary(("Added", added), ("Missing", missing), ("Different", different))

    case _ => Nil
  }

  def printNewLine =
    List(EmptyLine)

  /** show values as a string with a description */
  def printValues(description: String, values: Seq[Any]) =
    if values.nonEmpty then List((s"$description (${values.size})${values.map(notNullPair).mkString("\n", "\n", "\n\n")}").failure)
    else Nil

  /** print a short summary of differences between Seqs, Sets or Maps */
  def printSummary(descriptions: (String, Seq[String])*) =
    if descriptions.flatMap(_._2).mkString("\n").split("\n").length >= 50 then
      List((descriptions.map { case (name, values) => s"$name = ${values.size}" }.mkString(", ")).failure)
    else Nil

  def location(r: ResultStackTrace, fragmentLocation: Location, args: Arguments): String =
    val resultLocation = r.location(args.traceFilter)
    if resultLocation == "file:1" || resultLocation.isEmpty
      then s"(${fragmentLocation.fileName}:${fragmentLocation.lineNumber})"
      else s"($resultLocation)"

  def indentText(text: String, indentation: Int, indentationSize: Int) =
    if text.isEmpty then text
    else text.split("\n").toIndexedSeq.map((" " * (indentation * indentationSize)) + _).mkString("\n")
}


object TextPrinter {
  val default: TextPrinter =
    TextPrinter(Env())
}
