package org.specs2
package reporter

import matcher.DataTable
import control._
import specification.core._
import specification.process._
import text.NotNullStrings._
import text.Trim
import Trim._
import data.Fold
import Fold._
import execute._
import main.Arguments
import scalaz.stream._
import scalaz.stream.Process.{Env =>_,_}
import scalaz.concurrent.Task
import scalaz.concurrent.Task._
import LogLine._
import Functions._

/**
 * Prints the result of a specification execution to the console (using the line logger provided by the environment)
 *
 * At the end of the run the specification statistics are displayed as well.
 */
trait TextPrinter extends Printer {
  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit]  = Actions.unit
  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit] = Actions.unit

  def fold(env: Env, spec: SpecStructure) = new Fold[Fragment] {
    // statistics and indentation
    type S = (Stats, Int)

    lazy val logger = env.lineLogger
    lazy val args   = env.arguments <| spec.arguments
    
    lazy val logSink       = loggerSink(logger)
    lazy val lineStartSink = fragmentsSink(logger, spec.header)

    lazy val sink =
      lineStartSink.map { write: (LogLine => Task[Unit]) =>
        (current: (Fragment, S)) =>  printFragment(args)(current).map(write).eval.run
      }

    def prepare = Task.now(())
    def fold = Statistics.fold zip Indentation.fold
    def init = (Stats.empty, 0)

    def last(state: S) =
      printStats(spec.header, args)(state._1).to(logSink).run
  }

  /** run and shutdown the environment */
  def run(env: Env): SpecStructure => Unit = { spec: SpecStructure =>
    try     print(env)(spec).run
    finally env.shutdown
  }

  def shutdown = (ll: LineLogger) =>
    Task.delay(ll.close)

  def fragmentsSink(logger: LineLogger, header: SpecHeader): Sink[Task, LogLine] =
    io.resource(start(logger, header))(shutdown)(
      logger => Task.delay{ (line: LogLine) =>
        Task.now(line.log(logger))
      }
    )

  def loggerSink(logger: LineLogger): Sink[Task, LogLine] =
    io.resource(Task.delay(logger))(shutdown)(
      logger => Task.delay((line: LogLine) => Task.now(line.log(logger)))
    )

  def start(logger: LineLogger, header: SpecHeader): Task[LineLogger] =
    printHeader(header).map(_.log(logger)).run.map(_ => logger)

  def printHeader: SpecHeader => Process[Task, LogLine] = { header: SpecHeader =>
    emit(header.show).info
  }

  def printStats(header: SpecHeader, args: Arguments): Stats => Process[Task, LogLine] = { stats: Stats =>
    if ((args.xonly && stats.hasFailuresOrErrors || !args.xonly) && args.canShow("1")) {
      val title = if (header.show.isEmpty) "" else " "+header.show.trim

      printNewLine fby
      emit(s"Total for specification$title\n").info fby
        emit(stats.display(args)).info fby
        printNewLine
    } else emitNone
  }
import scalaz._, Scalaz._
  /** transform a stream of fragments into a stream of strings for printing */
  def printFragment(args: Arguments): ((Fragment, (Stats, Int))) => Process[Task, LogLine] = {
    case (fragment, (stats, indentation)) =>
      fragment match {
        // only print steps and actions if there are issues
        case Fragment(NoText, e, l) if e.isExecutable && !e.result.isSuccess =>
          printExecutable(NoText.show, e, args, indentation)

        case Fragment(d @ SpecificationRef(_, _, _, hidden), e, l)  =>
          if (!hidden) printExecutable(d.show, e, args, indentation)
          else emitNone

        case Fragment(d, e, l) if e.isExecutable && d != NoText =>
          printExecutable(d.show, e, args, indentation)

        case Fragment(Br, e, l) =>
          if (args.canShow("-")) printNewLine
          else emitNone

        case Fragment(Code(text), e, l) =>
          if (args.canShow("-")) emit(indentText(text, indentation, indentationSize(args))).info
          else emitNone

        case Fragment(d, e, l) =>
          if (args.canShow("-")) emit(indentText(d.show, indentation, indentationSize(args))).info
          else emitNone
      }
  }

  /** print an executed fragment: example, step, action */
  def printExecutable(text: String, execution: Execution, args: Arguments, indentation: Int): Process[Task, LogLine] = {

    if (args.canShow(execution.result.status)) {
      val show = indentText(showTime(statusAndDescription(text, execution.result)(args), execution, args), indentation, indentationSize(args))

      def printResult(r: Result) =
        r match {
          case err: execute.Error        => printError(show, err, args)
          case failure: execute.Failure  => printFailure(show, failure, args)
          case success: execute.Success  => printSuccess(show, success, args)
          case pending: execute.Pending  => printPending(show, pending, args)
          case skipped: execute.Skipped  => printSkipped(show, skipped, args)
          case other                     => printOther(show, other, args)
        }
      execution.result match {
        case DecoratedResult(t: DataTable, r) => printResult(r)
        case other                            => printResult(other)
      }
    } else emitNone
  }

  def printError(show: String, err: execute.Error, args: Arguments) =
    emit(show).error fby
      printMessage(args, show, ErrorLine.apply)(err)   fby
      printStacktrace(args, print = true, ErrorLine.apply)(err)

  def printFailure(show: String, failure: execute.Failure, args: Arguments) =
    emit(show).failure fby
      printMessage(args, show, FailureLine.apply)(failure) fby
      printStacktrace(args, print = args.failtrace, FailureLine.apply)(failure) fby
      printFailureDetails(args)(failure.details)

  def printSuccess(show: String, success: execute.Success, args: Arguments) = {
    val expected = if (success.exp.nonEmpty) "\n"+success.exp else ""
    if (expected.trim.nonEmpty) emit(show+expected).info
    else                        emit(show).info
  }

  def printPending(show: String, pending: execute.Pending, args: Arguments) = {
    val reason = if (pending.message.isEmpty) "PENDING" else pending.message

    if (reason.trim.nonEmpty) emit(show+" "+reason).info
    else                      emit(show).info

  }

  def printSkipped(show: String, skipped: execute.Skipped, args: Arguments) = {
    val reason =
      if (skipped.message != StandardResults.skipped.message)
        if (skipped.message.isEmpty) "SKIPPED" else skipped.message
      else skipped.message

    if (reason.trim.nonEmpty) emit(show+"\n"+reason).info
    else                      emit(show).info
  }

  def printOther(show: String, other: execute.Result, args: Arguments) = {
    emit(show).info
  }

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

  def printMessage(args: Arguments, description: String, as: String => LogLine): Result with ResultStackTrace => Process[Task, LogLine] = { result: Result with ResultStackTrace =>
    val margin = description.takeWhile(_ == ' ')+" "
    emit(as(result.message.split("\n").mkString(margin, "\n"+margin, "") + location(result, args)))
  }

  def printStacktrace(args: Arguments, print: Boolean, as: String => LogLine): Result with ResultStackTrace => Process[Task, LogLine] = { result: Result with ResultStackTrace =>
    if (print) Process(args.traceFilter(result.stackTrace).map(t => as(t.toString)):_*)
    else emitNone
  }

  /**
   * If the failure contains the expected and actual values, display them
   */
  def printFailureDetails(args: Arguments):  Details => Process[Task, LogLine] = {
    case FailureDetails(actual, expected) if args.diffs.show(actual, expected) =>
      val (actualDiff, expectedDiff) = args.diffs.showDiffs(actual, expected)
      emit("Actual:   " + actualDiff).failure fby
      emit("Expected: " + expectedDiff).failure fby
      (if (args.diffs.showFull) {
        emit("Actual (full):   " + actual).failure fby
        emit("Expected (full): " + expected).failure
      } else emitNone) fby
      emit("").info

    case details @ FailureSeqDetails(actual, expected) if args.diffs.showSeq(actual, expected, ordered = true) =>
      val (added, missing) = args.diffs.showSeqDiffs(actual, expected, ordered = true)

      printNewLine fby
      printValues("Added", added) fby printNewLine fby
      printValues("Missing", missing) fby printNewLine fby
      printSummary(("Added", added), ("Missing", missing))


    case details @ FailureSetDetails(actual, expected) if args.diffs.showSeq(actual.toSeq, expected.toSeq, ordered = false) =>
      val (added, missing) = args.diffs.showSeqDiffs(actual.toSeq, expected.toSeq, ordered = false)
      printNewLine fby
      printValues("Added", added) fby printNewLine fby
      printValues("Missing", missing) fby printNewLine fby
      printSummary(("Added", added), ("Missing", missing))

    case details @ FailureMapDetails(actual, expected) if args.diffs.showMap(actual, expected) =>
      val (added, missing, different) = args.diffs.showMapDiffs(actual, expected)
      printNewLine fby
      printValues("Added", added) fby printNewLine fby
      printValues("Missing", missing); printNewLine fby
      printValues("Different", different) fby printNewLine fby
      printSummary(("Added", added), ("Missing", missing), ("Different", different))

    case _ => emitNone
  }

  def printNewLine =
    emit(EmptyLine)

  /** show values as a string with a description */
  def printValues(description: String, values: Seq[Any]) =
    if (values.nonEmpty) emit(s"$description (${values.size})${values.map(notNullPair).mkString("\n", "\n", "\n\n")}").failure
    else emitNone

  /** print a short summary of differences between Seqs, Sets or Maps */
  def printSummary(descriptions: (String, Seq[String])*) =
    if (descriptions.flatMap(_._2).mkString("\n").split("\n").size >= 50)
      emit(descriptions.map { case (name, values) => s"$name = ${values.size}" }.mkString(", ")).failure
    else emitNone

  def location(r: ResultStackTrace, args: Arguments) = " ("+r.location(args.traceFilter)+")" unless r.location.isEmpty

  def indentText(text: String, indentation: Int, indentationSize: Int) = {
    if (text.isEmpty) text
    else text.split("\n").map((" " * (indentation * indentationSize)) + _).mkString("\n")
  }


  def emitNone: Process[Task, LogLine] = Process()
}

object TextPrinter extends TextPrinter

