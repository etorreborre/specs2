package org.specs2
package main

import scalaz.{Memo, Monoid, Scalaz}
import Scalaz._
import control._
import Exceptions._
import text._
import Split._
import data.SeparatedTags
import scala.Predef._

/**
 * This class holds all the options that are relevant for specs2 execution and reporting.
 *
 * The arguments are grouped along 4 functions:
 *
 * - select:  for the selection of what must be executed
 * - execute: for the execution of fragments
 * - store:   for the storing of execution results
 * - report:  for the reporting of results
 *
 */
case class
Arguments (
  select:        Select           = Select(),
  execute:       Execute          = Execute(),
  store:         Store            = Store(),
  report:        Report           = Report(),
  commandLine:   CommandLine      = CommandLine()
 ) extends ShowArgs {
  def ex: String                      = select.ex
  def include: String                 = select.include
  def exclude: String                 = select.exclude
  def keep(tags: String*)             = select.keep(tags:_*)
  def contain(tags: String*)          = select.contain(tags:_*)
  def hasFilter                       = select.hasFilter
  def wasIssue: Boolean               = select.wasIssue
  def was(s: String): Boolean         = select.was(s)
  def wasIsDefined: Boolean           = select.wasIsDefined
  def specName: String                = select.specName

  def plan: Boolean                   = execute.plan
  def skipAll: Boolean                = execute.skipAll

  def stopOnFail: Boolean             = execute.stopOnFail
  def stopOnSkip: Boolean             = execute.stopOnSkip
  def sequential: Boolean             = execute.sequential
  def isolated: Boolean               = execute.isolated
  def random: Boolean                 = execute.random
  def threadsNb: Int                  = execute.threadsNb

  def xonly: Boolean                  = report.xonly
  def canShow(s: String)              = report.canShow(s)

  def failtrace: Boolean              = report.failtrace
  def color: Boolean                  = report.color
  def colors: Colors                  = report.colors
  def showtimes: Boolean              = report.showtimes
  def offset: Int                     = report.offset
  def debugMarkdown: Boolean          = report.debugMarkdown
  def pegdownExtensions: Int          = report.pegdownExtensions
  def pegdownTimeout: Long            = report.pegdownTimeout
  def diffs: Diffs                    = report.diffs
  def fromSource: Boolean             = report.fromSource
  def traceFilter: StackTraceFilter   = report.traceFilter

  /** @return true if the command line contains a given string */
  def contains(a: String) = commandLine contains a
  /** alias for overrideWith */
  def <|(other: Arguments) = overrideWith(other)
  
  /**
   * @return a new Arguments object where the values of this are overriden with the values of other if defined
   */
  def overrideWith(other: Arguments): Arguments = {
    new Arguments(
      select.overrideWith(other.select),
      execute.overrideWith(other.execute),
      store.overrideWith(other.store),
      report.overrideWith(other.report),
      commandLine.overrideWith(other.commandLine)
    )
  }

  /**
   * shortcut methods to add ansi colors to some text depending on its status
   */
  def textColor   (s: String) = colors.text   (s, color)
  def successColor(s: String) = colors.success(s, color)
  def failureColor(s: String) = colors.failure(s, color)
  def errorColor  (s: String) = colors.error  (s, color)
  def pendingColor(s: String) = colors.pending(s, color)
  def skippedColor(s: String) = colors.skipped(s, color)
  def statsColor  (s: String) = colors.stats  (s, color)
  def removeColors(s: String) = colors.removeColors(s)

  /**
   * @return a new Arguments object with only some arguments on the command line
   */
  def commandLineFilter(included: String*) = copy(commandLine = commandLine.filter(included:_*))
  /**
   * @return a new Arguments object with some arguments removed from the command line
   */
  def commandLineFilterNot(excluded: String*) = copy(commandLine = commandLine.filterNot(excluded:_*))

  def verbose = commandLine.bool("verbose").isDefined

  override def toString = Seq(select, execute, report, commandLine).mkString("Arguments(", ", ", ")")

}
import main.{SystemProperties => sysProperties}

object Arguments extends Extract {
  
  /** @return new arguments from command-line arguments */
  def apply(implicit arguments: String*): Arguments = {
    if (arguments.isEmpty) new Arguments()
    else                   extract(CommandLine.splitValues(arguments.mkString(" ")), sysProperties)
  }

  private[specs2] def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Arguments = {
    new Arguments (
       select        = Select.extract,
       execute       = Execute.extract,
       store         = Store.extract,
       report        = Report.extract,
       commandLine   = CommandLine.extract
    )
  }
  
  implicit def ArgumentsMonoid: Monoid[Arguments] = new Monoid[Arguments] {
    def append(a1: Arguments, a2: =>Arguments) = a1 overrideWith a2
    val zero = Arguments()
  }

  /**
   * @return true if the flagList is empty or if it has
   */
  def hasFlags(s: String, flagList: Option[String]) = flagList match {
    case None        => true
    case Some(flags) => s.split("") forall flags.contains
  }
}

/**
 * Selection arguments
 */
private[specs2]
case class Select(
  _ex:            Option[String]           = None,
  _include:       Option[String]           = None,
  _exclude:       Option[String]           = None,
  _was:           Option[String]           = None,
  _specName:      Option[String]           = None) extends ShowArgs {

  import Arguments._
  
  def ex: String                    = _ex.getOrElse(".*")
  def include: String               = _include.getOrElse("")
  def exclude: String               = _exclude.getOrElse("")
  def keep(tags: String*)           = SeparatedTags(include, exclude).keep(tags)
  def contain(tags: String*)        = SeparatedTags(include, exclude).contain(tags)
  def hasFilter                     = Seq(_include, _exclude, _ex, _was, _specName).exists(_.isDefined)
  def wasIssue: Boolean             = was("x") || was("!")
  def was(s: String): Boolean       = hasFlags(s, _was)
  def wasIsDefined: Boolean         = _was.isDefined
  def specName: String              = _specName.getOrElse(".*Spec")

  def overrideWith(other: Select) = {
    new Select(
      other._ex              .orElse(_ex),
      other._include         .orElse(_include),
      other._exclude         .orElse(_exclude),
      other._was             .orElse(_was),
      other._specName        .orElse(_specName)
    )
  }

  override def toString = List(
    "ex"             -> _ex         ,
    "include"        -> _include    ,
    "exclude"        -> _exclude    ,
    "was"            -> _was        ,
    "specName"       -> _specName     ).flatMap(showArg).mkString("Select(", ", ", ")")
}

private[specs2]
object Select extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Select = {
    new Select (
       _ex            = value("ex", ".*"+(_:String)+".*"),
       _include       = value("include"),
       _exclude       = value("exclude"),
       _was           = value("was").orElse(bool("wasIssue").map(v => "x!")),
       _specName      = value("specName")
    )
  }
  val allValueNames = Seq("ex", "include", "exclude", "was", "wasIssue", "specName")
}

/**
 * Execution arguments
 */
private[specs2]
case class Execute(
  _plan:          Option[Boolean]          = None,
  _skipAll:       Option[Boolean]          = None,
  _stopOnFail:    Option[Boolean]          = None,
  _stopOnSkip:    Option[Boolean]          = None,
  _sequential:    Option[Boolean]          = None,
  _isolated:      Option[Boolean]          = None,
  _random:        Option[Boolean]          = None,
  _threadsNb:     Option[Int]              = None,
  _executor:      Option[String]           = None) extends ShowArgs {

  def plan: Boolean                 = _plan.getOrElse(false)
  def skipAll: Boolean              = _skipAll.getOrElse(false)
  def stopOnFail: Boolean           = _stopOnFail.getOrElse(false)
  def stopOnSkip: Boolean           = _stopOnSkip.getOrElse(false)
  def sequential: Boolean           = _sequential.getOrElse(false)
  def isolated: Boolean             = _isolated.getOrElse(false)
  def random: Boolean               = _random.getOrElse(false)
  def threadsNb: Int                = _threadsNb.getOrElse(Runtime.getRuntime.availableProcessors)
  def executor: String              = _executor.getOrElse("")

  def overrideWith(other: Execute) = {
    new Execute(
      other._plan            .orElse(_plan),
      other._skipAll         .orElse(_skipAll),
      other._stopOnFail      .orElse(_stopOnFail),
      other._stopOnSkip      .orElse(_stopOnSkip),
      other._sequential      .orElse(_sequential),
      other._isolated        .orElse(_isolated),
      other._random          .orElse(_random),
      other._threadsNb       .orElse(_threadsNb),
      other._executor        .orElse(_executor)
    )
  }

  override def toString =
    List(
    "plan"           -> _plan         ,
    "skipAll"        -> _skipAll      ,
    "stopOnFail"     -> _stopOnFail   ,
    "stopOnSkip"     -> _stopOnSkip   ,
    "sequential"     -> _sequential   ,
    "isolated"       -> _isolated     ,
    "threadsNb"      -> _threadsNb    ,
    "executor"       -> _executor     ).flatMap(showArg).mkString("Execute(", ", ", ")")

}
private[specs2]
object Execute extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Execute = {
    new Execute (
      _plan          = bool("plan"),
      _skipAll       = bool("skipAll"),
      _stopOnFail    = bool("stopOnFail"),
      _stopOnSkip    = bool("stopOnSkip"),
      _sequential    = bool("sequential"),
      _isolated      = bool("isolated"),
      _random        = bool("random"),
      _threadsNb     = int("threadsNb"),
      _executor      = value("executor")
    )
  }
  val allValueNames = Seq("plan", "skipAll", "stopOnFail", "stopOnSkip", "sequential", "isolated", "random", "threadsNb", "executor")
}

/**
 * Storing arguments
 */
private[specs2]
case class Store(
  _reset:         Option[Boolean]          = None,
  _never:         Option[Boolean]          = None) extends ShowArgs {

  def reset: Boolean              = _reset.getOrElse(false)
  def never: Boolean              = _never.getOrElse(false)

  def overrideWith(other: Store) = {
    new Store(
      other._reset         .orElse(_reset),
      other._never         .orElse(_never)
    )
  }

  override def toString =
    List(
    "reset"        -> _reset      ,
    "never"        -> _never      ).flatMap(showArg).mkString("Store(", ", ", ")")

}

private[specs2]
object Store extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Store = {
    new Store (
      _reset       = bool("resetStore"),
      _never       = bool("neverStore")
    )
  }

  val allValueNames = Seq("resetStore", "neverStore")
}

/**
 * Reporting arguments
 */
private[specs2]
case class Report(
  _showOnly:          Option[String]           = None,
  _failtrace:         Option[Boolean]          = None,
  _color:             Option[Boolean]          = None,
  _colors:            Option[Colors]           = None,
  _showtimes:         Option[Boolean]          = None,
  _offset:            Option[Int]              = None,
  _debugMarkdown:     Option[Boolean]          = None,
  _pegdownExtensions: Option[Int]              = None,
  _pegdownTimeout:    Option[Long]             = None,
  _streaming:         Option[Boolean]          = None,
  _diffs:             Option[Diffs]            = None,
  _fromSource:        Option[Boolean]          = None,
  _traceFilter:       Option[StackTraceFilter] = None,
  _checkUrls :        Option[Boolean]          = None,
  _notoc:             Option[Boolean]          = None,
  _notifier:          Option[String]           = None,
  _exporter:          Option[String]           = None) extends ShowArgs {

  import Arguments._
  
  def xonly: Boolean                 = canShow("x") && canShow("!") && !canShow("o*+")
  def canShow(s: String)             = hasFlags(s, _showOnly)
  def failtrace: Boolean             = _failtrace.getOrElse(false)
  def color: Boolean                 = _color.getOrElse(true)
  def colors: Colors                 = _colors.getOrElse(new SmartColors())
  def showtimes: Boolean             = _showtimes.getOrElse(false)
  def offset: Int                    = _offset.getOrElse(0)
  def debugMarkdown: Boolean         = _debugMarkdown.getOrElse(false)
  def pegdownExtensions: Int         = _pegdownExtensions.getOrElse(65535) // Extensions.ALL
  def pegdownTimeout: Long           = _pegdownTimeout.getOrElse(2000)
  def streaming: Boolean             = _streaming.getOrElse(false)
  def diffs: Diffs                   = _diffs.getOrElse(SmartDiffs())
  def fromSource: Boolean            = _fromSource.getOrElse(true)
  def traceFilter: StackTraceFilter  = _traceFilter.getOrElse(DefaultStackTraceFilter)
  def checkUrls: Boolean             = _checkUrls.getOrElse(false)
  def notoc: Boolean                 = _notoc.getOrElse(false)
  def hasToc: Boolean                = !notoc
  def notifier: String               = _notifier.getOrElse("")
  def exporter: String               = _exporter.getOrElse("")

  def overrideWith(other: Report) = {
    new Report(
      other._showOnly         .orElse(_showOnly),
      other._failtrace        .orElse(_failtrace),
      other._color            .orElse(_color),
      other._colors           .orElse(_colors),
      other._showtimes        .orElse(_showtimes),
      other._offset           .orElse(_offset),
      other._debugMarkdown    .orElse(_debugMarkdown),
      other._pegdownExtensions.orElse(_pegdownExtensions),
      other._pegdownTimeout   .orElse(_pegdownTimeout),
      other._streaming        .orElse(_streaming),
      other._diffs            .orElse(_diffs),
      other._fromSource       .orElse(_fromSource),
      other._traceFilter      .orElse(_traceFilter),
      other._checkUrls        .orElse(_checkUrls),
      other._notoc            .orElse(_notoc),
      other._notifier         .orElse(_notifier),
      other._exporter         .orElse(_exporter)
    )
  }

  override def toString = List(
    "showOnly"          -> _showOnly,
    "failtrace"         -> _failtrace,
    "color"             -> _color,
    "colors"            -> _colors,
    "showtimes"         -> _showtimes,
    "offset"            -> _offset,
    "debugMarkdown"     -> _debugMarkdown,
    "pegdownExtensions" -> _pegdownExtensions,
    "pegdownTimeout"    -> _pegdownTimeout,
    "streaming"         -> _streaming,
    "diffs"             -> _diffs,
    "fromSource"        -> _fromSource,
    "traceFilter"       -> _traceFilter,
    "checkUrls"         -> _checkUrls,
    "notoc"             -> _notoc,
    "notifier"          -> _notifier,
    "exporter"          -> _exporter).flatMap(showArg).mkString("Report(", ", ", ")")

}

private[specs2]
object Report extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Report = {
    new Report (
      _showOnly          = value("showOnly").orElse(bool("xOnly").map(v => "x!")),
      _failtrace         = bool("failTrace"),
      _color             = bool("color", "noColor"),
      _colors            = value("colors").map(SmartColors.fromArgs),
      _showtimes         = bool("showTimes"),
      _offset            = int("offset"),
      _debugMarkdown     = bool("debugMarkdown"),
      _pegdownExtensions = int("pegdownExtensions"),
      _pegdownTimeout    = long("pegdownTimeout"),
      _streaming         = bool("streaming"),
      _fromSource        = bool("fromSource"),
      _traceFilter       = bool("fullStackTrace").map(t=>NoStackTraceFilter).
                           orElse(value("traceFilter", IncludeExcludeStackTraceFilter.fromString(_))),
      _checkUrls         = bool("checkUrls"),
      _notoc             = bool("noToc"),
      _notifier          = value("notifier"),
      _exporter          = value("exporter")
    )
  }

  val allValueNames = Seq("showOnly", "xOnly", "failTrace", "color", "noColor", "colors", "offset", "showTimes",
                          "debugMarkdown", "pegdownExtensions", "pegdownTimeout", "streaming", "fromSource", "fullStackTrace", "traceFilter", "checkUrls", "noToc", "notifier", "exporter")
}
/**
 * Command-line arguments
 */
private[specs2]
case class CommandLine(_arguments: Seq[String] = Seq()) extends ShowArgs {

  def arguments: Seq[String] = _arguments
  def contains(a: String) = arguments contains a
  def value(name: String) = Arguments.value(name)(_arguments, SystemProperties)
  def isDefined(name: String) = value(name).isDefined
  def int(name: String) = Arguments.int(name)(_arguments, SystemProperties)
  def bool(name: String) = Arguments.bool(name)(_arguments, SystemProperties)
  def filter(included: String*) = copy(_arguments = arguments.filter(included.toSet.contains))
  def filterNot(excluded: String*) = copy(_arguments = arguments.filterNot(excluded.toSet.contains))
  def overrideWith(other: CommandLine) = copy(_arguments = if (other.arguments.isEmpty) this._arguments else other.arguments)

  override def toString = _arguments.mkString("CommandLine(", ", ", ")")
}

private[specs2]
object CommandLine extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): CommandLine =
    new CommandLine(_arguments = value("commandline").map(splitValues).getOrElse(Seq()) ++ arguments)

  val allValueNames = Select.allValueNames ++ Store.allValueNames ++ Execute.allValueNames ++ Report.allValueNames

  def splitValues(arguments: String): Seq[String] = arguments.splitDashed(allValueNames)
}

import Memo._

private[specs2]
trait Extract {
  def bool(name: String, mappedValue: Boolean = true)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => mappedValue).orElse(boolSystemProperty(name))
  }

  /**
   * memoize the boolean properties to improve performances
   */
  private val booleanProperties = immutableHashMapMemo[(String, SystemProperties), Option[Boolean]] { case (name, sp) =>
    sp.getPropertyAs[Boolean](name) orElse sp.getProperty(name).map(v => true)
  }
  def boolSystemProperty(name: String)(implicit sp: SystemProperties): Option[Boolean] = booleanProperties(name -> sp)

  def bool(name: String, negatedName: String)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    bool(negatedName, false) orElse bool(name)
  }
  def value[T](name: String, f: String => T)(implicit args: Seq[String], sp: SystemProperties): Option[T] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.equals(name.toLowerCase)).map(s => f(s._2)).orElse(valueSystemProperty(name, f))
  }
  def valueSystemProperty[T](name: String, f: String => T)(implicit sp: SystemProperties): Option[T] = {
    sp.getProperty(name).map(o => f(o.toString))
  }
  def value[T](name: String)(implicit args: Seq[String], sp: SystemProperties): Option[String] = value(name, identity _)
  def int(name: String)(implicit args: Seq[String], sp: SystemProperties): Option[Int] = {
    tryo(value(name)(args, sp).map(_.toInt)).getOrElse(None)
  }
  def long(name: String)(implicit args: Seq[String], sp: SystemProperties): Option[Long] = {
    tryo(value(name)(args, sp).map(_.toLong)).getOrElse(None)
  }

}

private[specs2]
trait ShowArgs {
  def showArg(a: (String, Option[_])) = a._2.map(a._1 +" = "+_)
}