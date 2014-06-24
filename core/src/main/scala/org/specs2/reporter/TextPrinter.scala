package org.specs2
package reporter

import specification._
import scalaz.stream.Process
import scalaz.stream.io
import scalaz.std.anyVal._
import scalaz.stream.Process.{Env => E, _}
import scalaz.concurrent.Task
import data.Processes._
import data.Fold
import execute._
import main.Arguments
import LogLine._
import text.Trim
import Trim._
import scalaz.concurrent.Task._
import reflect.Classes
import org.specs2.control._
import Actions._
import Fold._
import specification.core._
import specification.core._
import execute.FailureDetails
import specification.process._

trait TextPrinter extends Printer {

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

    def fold = Statistics.fold zip Indentation.fold
    def init = (Stats(), 0)

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

      emit(" \n").info fby
      emit(s"Total for specification$title\n").info fby
        emit(stats.display(args)).info fby
        emit("\n ").info
    } else emitNone
  }

  /** transform a stream of fragments into a stream of strings for printing */
  def printFragment(args: Arguments): ((Fragment, (Stats, Int))) => Process[Task, LogLine] = {
    case (fragment, (stats, indentation)) =>
      val indentationSize = args.commandLine.int("indentation").getOrElse(0)
      fragment match {
        case Fragment(RawText(t), e, l) if e.isRunnable =>
          printRunnable(t, e, args, indentation)

        case Fragment(Code(t), e, l) if e.isRunnable =>
          printRunnable(t, e, args, indentation)

        case Fragment(d, e, l) if e.isRunnable && !e.result.isSuccess =>
          printRunnable(d.show, e, args, indentation)

        case Fragment(Br, e, l) =>
          if (args.canShow("-")) emit(" \n").info
          else emitNone

        case Fragment(d, e, l) =>
          if (args.canShow("-")) emit(indentText(d.show, indentation, indentationSize)).info
          else emitNone
      }
  }


  def printRunnable(text: String, execution: Execution, args: Arguments, indentation: Int): Process[Task, LogLine] = {

    if (args.canShow(execution.result.status)) {
      val indentationSize = args.commandLine.int("indentation").getOrElse(0)
      val show = indentText(showTime(statusAndDescription(text, execution.result)(args), execution, args), indentation, indentationSize)

      execution.result match {
        case err: execute.Error        => printError(show, err, args)
        case failure: execute.Failure  => printFailure(show, failure, args)
        case success: execute.Success  => printSuccess(show, success, args)
        case pending: execute.Pending  => printPending(show, pending, args)
        case skipped: execute.Skipped  => printSkipped(show, skipped, args)
        case other                     => printOther(show, other, args)
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

  def showTime(description: String, execution: Execution, args: Arguments) = {
    val time = if (args.showtimes) " ("+execution.time+")" else ""
    description + time
  }


  def statusAndDescription(text: String, result: Result)(args: Arguments) = {
    val textLines = text.split("\n", -1)
    val firstLine = textLines.headOption.getOrElse("")
    val indentation = firstLine.takeWhile(_ == ' ').drop(2)

    val status = result.coloredStatus(args) + " "
    val decoratedFirstLine = indentation + status + firstLine.dropWhile(_ == ' ')
    val rest = textLines.drop(1).map(line => indentation + "   " + line)
    (decoratedFirstLine +: rest).mkString("\n")
  }

  def printMessage(args: Arguments, description: String, as: String => LogLine): Result with ResultStackTrace => Process[Task, LogLine] = { result: Result with ResultStackTrace =>
    val margin = description.takeWhile(_ == ' ')+" "
    emit(as(result.message.split("\n").mkString(margin, "\n"+margin, "") + location(result, args)))
  }

  def printStacktrace(args: Arguments, print: Boolean, as: String => LogLine): Result with ResultStackTrace => Process[Task, LogLine] = { result: Result with ResultStackTrace =>
    if (print) Process(args.traceFilter(result.stackTrace).map(t => as(t.toString)):_*)
    else emitNone
  }

  def printFailureDetails(args: Arguments):  Details => Process[Task, LogLine] = {
    case FailureDetails(expected, actual) if args.diffs.show(expected, actual) =>
      val (expectedDiff, actualDiff) = args.diffs.showDiffs(expected, actual)
      emit("Expected: " + expectedDiff).failure fby
      emit("Actual:   " + actualDiff).failure fby
      (if (args.diffs.showFull) {
        emit("Expected (full): " + expected).failure
        emit("Actual (full):   " + actual).failure
      } else emitNone) fby
      emit("").info

    case _ => emitNone
  }

  def location(r: ResultStackTrace, args: Arguments) = " ("+r.location(args.traceFilter)+")" unless r.location.isEmpty

  def indentText(text: String, indentation: Int, indentationSize: Int) =
    text.split("\n").map((" " * (indentation * indentationSize)) + _).mkString("\n")

  def emitNone: Process[Task, LogLine] = Process()
}

object TextPrinter extends TextPrinter

