package org.specs2
package main

import main.{SystemProperties as sysProperties}
import org.specs2.fp.*
import control.*
import text.*
import scala.Predef.*
import scala.concurrent.duration.FiniteDuration

/** This class holds all the options that are relevant for specs2 execution and reporting.
  *
  * The arguments are grouped along 4 functions:
  *
  *   - select: for the selection of what must be executed
  *   - execute: for the execution of fragments
  *   - store: for the storing of execution results
  *   - report: for the reporting of results
  */
case class Arguments(
    select: Select = Select(),
    execute: Execute = Execute(),
    store: Store = Store(),
    report: Report = Report(),
    commandLine: CommandLine = CommandLine(),
    unknown: List[String] = List()
) extends ShowArgs {
  def ex: String = select.ex
  def include: String = select.include
  def exclude: String = select.exclude
  def keep(tags: String*) = select.keep(tags*)
  def contain(tags: String*) = select.contain(tags*)
  def hasFilter = select.hasFilter
  def was(s: String): Boolean = select.was(s)
  def wasIsDefined: Boolean = select.wasIsDefined

  def plan: Boolean = execute.plan
  def skipAll: Boolean = execute.skipAll

  def stopOnFail: Boolean = execute.stopOnFail
  def stopOnError: Boolean = execute.stopOnError
  def stopOnIssue: Boolean = execute.stopOnIssue
  def stopOnSkip: Boolean = execute.stopOnSkip
  def sequential: Boolean = execute.sequential
  def sequentialRandom: Boolean = execute.sequentialRandom
  def threadsNb: Int = execute.threadsNb
  def specs2ThreadsNb: Int = execute.specs2ThreadsNb
  def discardRejectedFutures: Boolean = execute.discardRejectedFutures
  def timeFactor: Int = execute.timeFactor
  def timeout: Option[FiniteDuration] = execute.timeout
  def setTimeout(t: FiniteDuration) = copy(execute = execute.setTimeout(t))
  def batchSize: Int = execute.batchSize
  def scheduledThreadsNb: Int = execute.scheduledThreadsNb
  def useCustomClassLoader: Boolean = execute.useCustomClassLoader

  def xonly: Boolean = report.xonly
  def canShow(s: String) = report.canShow(s)

  def failtrace: Boolean = report.failtrace
  def color: Boolean = report.color
  def colors: Colors = report.colors
  def showtimes: Boolean = report.showtimes
  def offset: Int = report.offset
  def diffs: Diffs = report.diffs
  def traceFilter: StackTraceFilter = report.traceFilter

  /** @return true if a switch is present or a flag is set */
  def isSet(a: String) = commandLine `isSet` a

  /** alias for overrideWith */
  def <|(other: Arguments) = overrideWith(other)

  /** @return
    *   a new Arguments object where the values of this are overridden with the values of other if defined
    */
  def overrideWith(other: Arguments): Arguments =
    new Arguments(
      select.overrideWith(other.select),
      execute.overrideWith(other.execute),
      store.overrideWith(other.store),
      report.overrideWith(other.report),
      commandLine.overrideWith(other.commandLine)
    )

  /** shortcut methods to add ansi colors to some text depending on its status
    */
  def textColor(s: String) = colors.text(s, color)
  def successColor(s: String) = colors.success(s, color)
  def failureColor(s: String) = colors.failure(s, color)
  def errorColor(s: String) = colors.error(s, color)
  def pendingColor(s: String) = colors.pending(s, color)
  def skippedColor(s: String) = colors.skipped(s, color)
  def statsColor(s: String) = colors.stats(s, color)
  def removeColors(s: String) = colors.removeColors(s)

  /** @return
    *   a new Arguments object with only some arguments on the command line
    */
  def commandLineFilter(included: String*) = copy(commandLine = commandLine.filter(included*))

  /** @return
    *   a new Arguments object with some arguments removed from the command line
    */
  def commandLineFilterNot(excluded: String*) = copy(commandLine = commandLine.filterNot(excluded*))

  def verbose = commandLine.bool("verbose").isDefined

  override def toString = Seq(select, execute, report, commandLine).mkString("Arguments(", ", ", ")")

  def reportUnknown(): Unit =
    if (verbose && unknown.nonEmpty)
      println("Unknown argument values: " + unknown.mkString(", "))
}

object Arguments extends Extract:

  /** @return new arguments from command-line arguments */
  def apply(arguments: String*): Arguments =
    extract(using CommandLine.splitValues(arguments), sysProperties)

  /** create Arguments from a string by splitting it on spaces */
  def split(arguments: String): Arguments =
    Arguments(arguments.split(" ")*)

  private[specs2] def extract(using arguments: Seq[String], systemProperties: SystemProperties): Arguments =
    new Arguments(
      select = Select.extract,
      execute = Execute.extract,
      store = Store.extract,
      report = Report.extract,
      commandLine = CommandLine.extract,
      unknown = CommandLine.unknownArguments
    )

  implicit def ArgumentsMonoid: Monoid[Arguments] = new Monoid[Arguments]:
    def append(a1: Arguments, a2: =>Arguments) = a1.overrideWith(a2)
    val zero = Arguments()

  /** @return
    *   true if the flagList is empty or if it has
    */
  def hasFlags(s: String, flagList: Option[String]) = flagList match
    case Some(flags) => s.split("") forall flags.contains
    case _           => true

trait ShowArgs:
  def showArg(a: (String, Option[?])) = a._2.map(a._1 + " = " + _)
