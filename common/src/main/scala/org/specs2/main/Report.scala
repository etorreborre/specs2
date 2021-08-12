package org.specs2
package main

import control.*
import text.*

/** Reporting arguments
  */
case class Report(
    _showOnly: Option[String] = None,
    _failtrace: Option[Boolean] = None,
    _color: Option[Boolean] = None,
    _colors: Option[Colors] = None,
    _showtimes: Option[Boolean] = None,
    _offset: Option[Int] = None,
    _diffs: Option[Diffs] = None,
    _traceFilter: Option[StackTraceFilter] = None,
    _checkUrls: Option[Boolean] = None,
    _notoc: Option[Boolean] = None,
    _notifier: Option[String] = None,
    _printer: Option[String] = None
) extends ShowArgs:

  import Arguments.*

  def xonly: Boolean = Report.xonlyFlags.forall(c => canShow(c.toString)) && !canShow("o*+")
  def canShow(s: String) = hasFlags(s, _showOnly)
  def failtrace: Boolean = _failtrace.getOrElse(false)
  def color: Boolean = _color.getOrElse(true)
  def colors: Colors = _colors.getOrElse(new MappedColors())
  def showtimes: Boolean = _showtimes.getOrElse(false)
  def offset: Int = _offset.getOrElse(0)
  def diffs: Diffs = _diffs.getOrElse(SmartDiffs())
  def traceFilter: StackTraceFilter = _traceFilter.getOrElse(DefaultStackTraceFilter)
  def checkUrls: Boolean = _checkUrls.getOrElse(false)
  def notoc: Boolean = _notoc.getOrElse(false)
  def hasToc: Boolean = !notoc
  def notifier: String = _notifier.getOrElse("")
  def printer: String = _printer.getOrElse("")

  def overrideWith(other: Report) =
    new Report(
      other._showOnly.orElse(_showOnly),
      other._failtrace.orElse(_failtrace),
      other._color.orElse(_color),
      other._colors.orElse(_colors),
      other._showtimes.orElse(_showtimes),
      other._offset.orElse(_offset),
      other._diffs.orElse(_diffs),
      other._traceFilter.orElse(_traceFilter),
      other._checkUrls.orElse(_checkUrls),
      other._notoc.orElse(_notoc),
      other._notifier.orElse(_notifier),
      other._printer.orElse(_printer)
    )

  override def toString = List(
    "showOnly" -> _showOnly,
    "failtrace" -> _failtrace,
    "color" -> _color,
    "colors" -> _colors,
    "showtimes" -> _showtimes,
    "offset" -> _offset,
    "diffs" -> _diffs,
    "traceFilter" -> _traceFilter,
    "checkUrls" -> _checkUrls,
    "notoc" -> _notoc,
    "notifier" -> _notifier,
    "printer" -> _printer
  ).flatMap(showArg).mkString("Report(", ", ", ")")

object Report extends Extract:
  def extract(using arguments: Seq[String], systemProperties: SystemProperties): Report =
    new Report(
      _showOnly = value("showOnly").orElse(bool("xOnly").map(_ => xonlyFlags)),
      _failtrace = bool("failTrace"),
      _color = bool("color", "noColor"),
      _colors = value("colors").map(MappedColors.fromArgs).orElse(value("colorsclass").flatMap(instance[Colors])),
      _showtimes = bool("showTimes"),
      _offset = int("offset"),
      _diffs = value("smartdiffs")
        .flatMap(parameters => SmartDiffs.fromString(parameters).toOption)
        .orElse(value("diffsclass").flatMap(instance)),
      _traceFilter = bool("fullStackTrace")
        .map(t => NoStackTraceFilter)
        .orElse(value("traceFilter", IncludeExcludeStackTraceFilter.fromString)),
      _checkUrls = bool("checkUrls"),
      _notoc = bool("noToc"),
      _notifier = value("notifier"),
      _printer = value("printer")
    )

  val xonlyFlags = "#x!"
  val allFlags = "#1x!+-o*"

  val allArguments: Seq[ArgumentType] =
    Seq(
      ValuedArgument("showOnly"),
      BooleanArgument("xOnly"),
      BooleanArgument("failTrace"),
      BooleanArgument("color"),
      BooleanArgument("noColor"),
      BooleanArgument("verbose"),
      ValuedArgument("colors"),
      BooleanArgument("showTimes"),
      ValuedArgument("offset"),
      ValuedArgument("smartdiffs"),
      ValuedArgument("diffsclass"),
      BooleanArgument("fullStackTrace"),
      ValuedArgument("traceFilter"),
      BooleanArgument("checkUrls"),
      BooleanArgument("noToc"),
      ValuedArgument("notifier"),
      ValuedArgument("printer")
    )
