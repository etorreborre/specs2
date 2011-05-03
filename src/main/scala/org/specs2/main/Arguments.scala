package org.specs2
package main

import scalaz.Monoid
import control._
import Exceptions._
import text.{DiffShortener, EditDistance}

/**
 * This class holds all the options that are relevant for specs2 execution and reporting.
 */
private[specs2]  
case class Arguments (
  _ex:            Option[String]           = None,
  _xonly:         Option[Boolean]          = None,
  _include:       Option[String]           = None,
  _exclude:       Option[String]           = None,
  _plan:          Option[Boolean]          = None,
  _skipAll:       Option[Boolean]          = None,
  _stopOnFail:    Option[Boolean]          = None,
  _failtrace:     Option[Boolean]          = None,
  _color:         Option[Boolean]          = None,
  _noindent:      Option[Boolean]          = None,
  _showlevel:     Option[Boolean]          = None,
  _showtimes:     Option[Boolean]          = None,
  _offset:        Option[Int]              = None,
  _specName:      Option[String]           = None,
  _sequential:    Option[Boolean]          = None,
  _threadsNb:     Option[Int]              = None,
  _markdown:      Option[Boolean]          = None,
  _debugMarkdown: Option[Boolean]          = None,
  _diffs:         Option[Diffs]            = None,
  _fromSource:    Option[Boolean]          = None,
  _traceFilter:   Option[StackTraceFilter] = None,
  _commandLine:   Seq[String]              = Nil
 ) {
  def ex: String                    = _ex.getOrElse(".*")
  def xonly: Boolean                = _xonly.getOrElse(false)
  def include: String               = _include.getOrElse("")
  def exclude: String               = _exclude.getOrElse("")
  def plan: Boolean                 = _plan.getOrElse(false)
  def skipAll: Boolean              = _skipAll.getOrElse(false)
  def stopOnFail: Boolean           = _stopOnFail.getOrElse(false)
  def failtrace: Boolean            = _failtrace.getOrElse(false)
  def color: Boolean                = _color.getOrElse(true)
  def noindent: Boolean             = _noindent.getOrElse(false)
  def showlevel: Boolean            = _showlevel.getOrElse(false)
  def showtimes: Boolean            = _showtimes.getOrElse(false)
  def offset: Int                   = _offset.getOrElse(0)
  def specName: String              = _specName.getOrElse(".*Spec")
  def sequential: Boolean           = _sequential.getOrElse(false)
  def threadsNb: Int                = _threadsNb.getOrElse(Runtime.getRuntime.availableProcessors)
  def markdown: Boolean             = _markdown.getOrElse(true)
  def debugMarkdown: Boolean        = _debugMarkdown.getOrElse(false)
  def diffs: Diffs                  = _diffs.getOrElse(SmartDiffs())
  def fromSource: Boolean           = _fromSource.getOrElse(true)
  def traceFilter: StackTraceFilter = _traceFilter.getOrElse(DefaultStackTraceFilter)
  def commandLine: Seq[String]      = _commandLine

  /** @return true if the command line contains a given string */
  def contains(a: String) = commandLine contains a
  /** @alias for overrideWith */
  def <|(other: Arguments) = overrideWith(other)
  
  /**
   * @return a new Arguments object where the values of this are overriden with the values of other if defined
   */
  def overrideWith(other: Arguments): Arguments = {
    new Arguments(
      other._ex              .orElse(_ex),
      other._xonly           .orElse(_xonly),
      other._include         .orElse(_include),
      other._exclude         .orElse(_exclude),
      other._plan            .orElse(_plan),
      other._skipAll         .orElse(_skipAll),
      other._stopOnFail      .orElse(_stopOnFail),
      other._failtrace       .orElse(_failtrace),
      other._color           .orElse(_color),
      other._noindent        .orElse(_noindent),
      other._showlevel       .orElse(_showlevel),
      other._showtimes       .orElse(_showtimes),
      other._offset          .orElse(_offset),
      other._specName        .orElse(_specName),
      other._sequential      .orElse(_sequential),
      other._threadsNb       .orElse(_threadsNb),
      other._markdown        .orElse(_markdown),
      other._debugMarkdown   .orElse(_debugMarkdown),
      other._diffs           .orElse(_diffs),
      other._fromSource      .orElse(_fromSource),
      other._traceFilter     .orElse(_traceFilter),
      if (other._commandLine.isEmpty) _commandLine else other._commandLine
    )
  }
  override def toString = {
    "Arguments("      +
    List(
    "ex"             -> _ex           ,
    "xonly"          -> _xonly        ,
    "include"        -> _include      ,
    "exclude"        -> _exclude      ,
    "plan"           -> _plan         ,
    "skipAll"        -> _skipAll      ,
    "stopOnFail"     -> _stopOnFail   ,
    "failtrace"      -> _failtrace    ,
    "color"          -> _color        ,
    "noindent"       -> _noindent     ,
    "showlevel"      -> _showlevel    ,
    "showtimes"      -> _showtimes    ,
    "offset"         -> _offset       ,
    "specName"       -> _specName     ,
    "sequential"     -> _sequential   ,
    "threadsNb"      -> _threadsNb    ,
    "markdown"       -> _markdown     ,
    "debugMarkdown"  -> _debugMarkdown,
    "diffs"          -> _diffs,
    "fromSource"     -> _fromSource,
    "traceFilter"    -> _traceFilter,
    "commandLine"    -> (if (_commandLine.isEmpty) None else Some(_commandLine.mkString(", ")))
    ).flatMap(showArg).mkString(", ") + ") "
  }

  private def showArg(a: (String, Option[_])) = a._2.map(a._1 +" = "+_)
} 
import main.{SystemProperties => sysProperties}

private[specs2]  
object Arguments {
  
  /** @return new arguments from command-line arguments */
  def apply(implicit arguments: String*): Arguments = {
    extract(arguments, sysProperties)
  }
  private[specs2] def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Arguments = {
    new Arguments (
       _ex            = value("ex", ".*"+(_:String)+".*"),
       _xonly         = bool("xonly"),
       _include       = value("include"),
       _exclude       = value("exclude"),
       _plan          = bool("plan"),
       _skipAll       = bool("skipall"),
       _stopOnFail    = bool("stoponfail"),
       _failtrace     = bool("failtrace"),
       _color         = bool("color", "nocolor"),
       _noindent      = bool("noindent"),
       _showlevel     = bool("showlevel"),
       _showtimes     = bool("showtimes"),
       _offset        = int("offset"),
       _specName      = value("specname"),
       _sequential    = bool("sequential"),
       _threadsNb     = int("threadsnb"),
       _markdown      = bool("markdown", "nomarkdown"),
       _debugMarkdown = bool("debugmarkdown"),
       _fromSource    = bool("fromsource"),
       _traceFilter   = bool("fullstacktrace").map(t=>NoStackTraceFilter).
                        orElse(value("tracefilter", IncludeExcludeStackTraceFilter.fromString(_))),
       _commandLine   = arguments
    )
  }
  
  private def bool(name: String, mappedValue: Boolean = true)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => mappedValue).orElse(boolSystemProperty(name, mappedValue))
  }
  private def boolSystemProperty(name: String, mappedValue: Boolean = true)(implicit sp: SystemProperties): Option[Boolean] = {
    sp.getProperty(name).map(a => mappedValue)
  }
  private def bool(name: String, negatedName: String)(implicit args: Seq[String], sp: SystemProperties): Option[Boolean] = {
    bool(negatedName, false) orElse bool(name)
  }
  private def value[T](name: String, f: String => T)(implicit args: Seq[String], sp: SystemProperties): Option[T] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(s => f(s._2)).orElse(valueSystemProperty(name, f))
  }
  private def valueSystemProperty[T](name: String, f: String => T)(implicit sp: SystemProperties): Option[T] = {
    sp.getProperty(name).map(o => f(o.toString))
  }
  private def value[T](name: String)(implicit args: Seq[String], sp: SystemProperties): Option[String] = value(name, identity _)
  private def int(name: String)(implicit args: Seq[String], sp: SystemProperties): Option[Int] = {
    tryo(value(name)(args, sp).map(_.toInt).get)
  }

  implicit def ArgumentsMonoid: Monoid[Arguments] = new Monoid[Arguments] {
    def append(a1: Arguments, a2: =>Arguments) = a1 overrideWith a2
    val zero = Arguments()
  }
}

/**
 * this trait is used to define and compute the differences between strings (used by the reporters)
 */
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean
  /** @return true if the differences must be shown for 2 different strings */
  def show(expected: String, actual: String): Boolean
  /** @return the diffs */
  def showDiffs(expected: String, actual: String): (String, String)
  /** @return true if the full strings must also be shown */
  def showFull: Boolean
  /** @return the separators to use*/
  def separators: String
}

/**
 * The SmartDiffs class holds all the required parameters to show differences between 2 strings using the edit distance
 * algorithm
 */
case class SmartDiffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, shortenSize: Int = 5, diffRatio: Int = 30, showFull: Boolean = false) extends Diffs {
  import EditDistance._

  def show(expected: String, actual: String): Boolean = show && Seq(expected, actual).exists(_.size >= triggerSize)
  def showDiffs(expected: String, actual: String) = {
    if (editDistance(expected, actual).doubleValue / (expected.size + actual.size) < diffRatio.doubleValue / 100)
      showDistance(expected, actual, separators, shortenSize)
    else
      (expected, actual)
  }
}

