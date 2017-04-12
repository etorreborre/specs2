package org.specs2
package reporter

import matcher.DataTable
import control._
import origami._
import eff.all._
import specification.core._
import specification.process._
import text.NotNullStrings._
import text.Trim
import time._
import Trim._
import execute._
import main.Arguments
import LogLine._
import org.specs2.fp.syntax._
import Actions._
import scala.concurrent._


/**
 * Prints the result of a specification execution to the console (using the line logger provided by the environment)
 *
 * At the end of the run the specification statistics are displayed as well.
 */
trait TextPrinter extends Printer {
  def prepare(env: Env, specifications: List[SpecStructure]): Action[Unit]  = Actions.unit
  def finalize(env: Env, specifications: List[SpecStructure]): Action[Unit] = Actions.unit

  def sink(env: Env, spec: SpecStructure): AsyncSink[Fragment] = {
    // statistics and indentation
    type S = ((Stats, Int), SimpleTimer)

    val values = Statistics.fold zip Indentation.fold zip SimpleTimer.timerFold

    lazy val logger = env.lineLogger
    lazy val args   = env.arguments <| spec.arguments

    lazy val sink: AsyncSink[(Fragment, S)] =
      Folds.fromStart(start(logger, spec.header, args)) *>
        linesLoggerSink(logger, spec.header, args).
          contramap[(Fragment, S)](printFragment(args))

    (values.into[ActionStack] observeWithState sink).mapFlatten(printFinalStats(spec, args, logger))
  }

  /** run and shutdown the environment */
  def run(env: Env)(implicit ec: ExecutionContext): SpecStructure => Unit = { spec: SpecStructure =>
    try     { runAction(print(env)(spec)); () }
    finally env.shutdown
  }

  def linesLoggerSink(logger: LineLogger, header: SpecHeader, args: Arguments): AsyncSink[List[LogLine]] =
    Folds.fromSink[ActionStack, List[LogLine]](lines =>
      pure(lines.foreach(_.log(logger))))

  def start(logger: LineLogger, header: SpecHeader, args: Arguments): Action[LineLogger] =
    asyncDelayAction(printHeader(args)(header).foreach(_.log(logger))).as(logger)

  def printFinalStats(spec: SpecStructure, args: Arguments, logger: LineLogger): (((Stats, Int), SimpleTimer)) => Action[Unit] = { case ((stats, _), timer) =>
    asyncDelayAction(printStats(spec.header, args, stats, timer).foreach(_.log(logger))) >>
    asyncDelayAction(logger.close)
  }

  def printHeader(args: Arguments): SpecHeader => List[LogLine] = { header: SpecHeader =>
    if (args.canShow("#")) List(header.show.info)
    else Nil
  }

  def printStats(header: SpecHeader, args: Arguments, stats: Stats, timer: SimpleTimer): List[LogLine] = {
    if ((args.xonly && stats.hasFailuresOrErrors || !args.xonly) && args.canShow("1")) {
      val title = if (header.show.isEmpty) "" else " "+header.show.trim

      printNewLine ++
      printNewLine ++
      List(
        s"Total for specification$title\n".info,
        stats.copy(timer = timer).display(args).info) ++
      printNewLine ++
      printNewLine
    } else Nil
  }

  /** transform a stream of fragments into a stream of strings for printing */
  def printFragment(args: Arguments): ((Fragment, ((Stats, Int), SimpleTimer))) => List[LogLine] = {
    case (fragment, ((stats, indentation), _)) =>
      fragment match {
        // only print steps and actions if there are issues
        case Fragment(NoText, e, l) if e.isExecutable && !e.result.isSuccess =>
          printExecutable(NoText, e, args, indentation)

        case Fragment(d @ SpecificationRef(_, _, _, _, hidden, muted), e, l)  =>
          if (!hidden)
            if (e.isExecutable && !muted) printExecutable(d, e, args, indentation)
            else                          List(d.show.info)
          else Nil

        case Fragment(d, e, l) if e.isExecutable && d != NoText =>
          printExecutable(d, e, args, indentation)

        case Fragment(Br, e, l) =>
          if (args.canShow("-")) printNewLine
          else Nil

        case Fragment(Code(text), e, l) =>
          if (args.canShow("-")) List(indentText(text, indentation, indentationSize(args)).info)
          else Nil

        case Fragment(d, e, l) =>
          if (args.canShow("-")) List(indentText(d.show, indentation, indentationSize(args)).info)
          else Nil
      }
  }

  /** print an executed fragment: example, step, action */
  def printExecutable(description: Description, execution: Execution, args: Arguments, indentation: Int): List[LogLine] = {

    if (args.canShow(execution.result.status)) {
      val text = description.show
      val show = indentText(showTime(statusAndDescription(text, execution.result)(args), execution, args), indentation, indentationSize(args))

      def printResult(desc: String, r: Result) =
        r match {
          case err: execute.Error        => printError(desc, err, args)
          case failure: execute.Failure  => printFailure(desc, failure, args)
          case success: execute.Success  => printSuccess(desc, success, args)
          case pending: execute.Pending  => printPending(desc, pending, args)
          case skipped: execute.Skipped  => printSkipped(desc, skipped, args)
          case other                     => printOther(desc, other, args)
        }

      execution.result match {
        case DecoratedResult(t: DataTable, r) =>
          // display the full table if it is an auto-example
          if (Description.isCode(description))
            printResult(indentText(r.message, indentation, indentationSize(args)), r.updateMessage(""))
          else
            printResult(show, r)

        case other => printResult(show, other)
      }
    } else Nil
  }

  def printError(show: String, err: execute.Error, args: Arguments): List[LogLine] =
    List(show.error) ++
    printMessage(args, show, ErrorLine.apply)(err) ++
    printStacktrace(args, print = true, ErrorLine.apply)(err) ++
    (if (err.exception.getCause != null) printError("CAUSED BY", execute.Error(err.exception.getCause), args)
     else List())

  def printFailure(show: String, failure: execute.Failure, args: Arguments): List[LogLine] =
    List(show.failure) ++
    printMessage(args, show, FailureLine.apply)(failure) ++
    printStacktrace(args, print = args.failtrace, FailureLine.apply)(failure) ++
    printFailureDetails(args)(failure.details)

  def printSuccess(show: String, success: execute.Success, args: Arguments): List[LogLine] = {
    val expected = if (success.exp.nonEmpty) "\n"+success.exp else ""
    if (expected.trim.nonEmpty) List((show+expected).info)
    else                        List(show.info)
  }

  def printPending(show: String, pending: execute.Pending, args: Arguments): List[LogLine] = {
    val reason = if (pending.message.isEmpty) "PENDING" else pending.message

    if (reason.trim.nonEmpty) List((show+" "+reason).info)
    else                      List(show.info)

  }

  def printSkipped(show: String, skipped: execute.Skipped, args: Arguments): List[LogLine] = {
    val reason =
      if (skipped.message != StandardResults.skipped.message)
        if (skipped.message.isEmpty) "SKIPPED" else skipped.message
      else skipped.message

    if (reason.trim.nonEmpty) List((show+"\n"+reason).info)
    else                      List(show.info)
  }

  def printOther(show: String, other: execute.Result, args: Arguments): List[LogLine] =
    List(show.info)

  /** show execution times if the showtimes argument is true */
  def showTime(description: String, execution: Execution, args: Arguments) = {
    val time = if (args.showtimes) " ("+execution.time+")" else ""
    description + time
  }

  def statusAndDescription(text: String, result: Result)(args: Arguments) = {
    val textLines = text.trimEnclosing("`").trimEnclosing("```").split("\n", -1) // trim markdown code marking
    val firstLine = textLines.headOption.getOrElse("")
    val (indentation, line) = firstLine.span(_ == ' ')
    val status = result.coloredStatus(args) + " "
    val decoratedFirstLine = indentation + status + (if (Seq("*", "-").exists(line.startsWith)) line.drop(2) else line)

    val rest = textLines.drop(1).map(l => s"  $l")
    (decoratedFirstLine +: rest).mkString("\n")
  }

  def indentationSize(args: Arguments): Int =
    args.commandLine.int("indentation").getOrElse(2)

  def printMessage(args: Arguments, description: String, as: String => LogLine): Result with ResultStackTrace => List[LogLine] = { result: Result with ResultStackTrace =>
    val margin = description.takeWhile(_ == ' ')+" "
    List(as(result.message.split("\n").mkString(margin, "\n"+margin, "") + location(result, args)))
  }

  def printStacktrace(args: Arguments, print: Boolean, as: String => LogLine): Result with ResultStackTrace => List[LogLine] = { result: Result with ResultStackTrace =>
    if (print) args.traceFilter(result.stackTrace).map(t => as(t.toString)).toList
    else Nil
  }

  /**
   * If the failure contains the expected and actual values, display them
   */
  def printFailureDetails(args: Arguments):  Details => List[LogLine] = {
    case FailureDetails(actual, expected) if args.diffs.show(actual, expected) =>
      val (actualDiff, expectedDiff) = args.diffs.showDiffs(actual, expected)
      val shortDiff =
        if (actualDiff != expectedDiff)
          List(("Actual:   " + actualDiff).failure,
               ("Expected: " + expectedDiff).failure)
        else List()

      val fullDiff =
        (if (args.diffs.showFull)
          List(("Actual (full):   " + actual).failure,
               ("Expected (full): " + expected).failure)
        else Nil)

      shortDiff ++ fullDiff ++
      List("".info)

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
    if (values.nonEmpty) List((s"$description (${values.size})${values.map(notNullPair).mkString("\n", "\n", "\n\n")}").failure)
    else Nil

  /** print a short summary of differences between Seqs, Sets or Maps */
  def printSummary(descriptions: (String, Seq[String])*) =
    if (descriptions.flatMap(_._2).mkString("\n").split("\n").size >= 50)
      List((descriptions.map { case (name, values) => s"$name = ${values.size}" }.mkString(", ")).failure)
    else Nil

  def location(r: ResultStackTrace, args: Arguments) = " ("+r.location(args.traceFilter)+")" unless r.location.isEmpty

  def indentText(text: String, indentation: Int, indentationSize: Int) = {
    if (text.isEmpty) text
    else text.split("\n").map((" " * (indentation * indentationSize)) + _).mkString("\n")
  }

}

object TextPrinter extends TextPrinter

