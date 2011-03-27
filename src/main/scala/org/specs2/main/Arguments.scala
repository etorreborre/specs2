package org.specs2
package main

import scalaz.Monoid
import control.Exceptions._

/**
 * This class holds all the options that are relevant for specs2 execution and reporting.
 */
private[specs2]  
case class Arguments (
  _ex:            Option[String]  = None,
  _xonly:         Option[Boolean] = None,
  _include:       Option[String]  = None,
  _exclude:       Option[String]  = None,
  _plan:          Option[Boolean] = None,
  _skipAll:       Option[Boolean] = None,
  _failtrace:     Option[Boolean] = None,
  _color:         Option[Boolean] = None,
  _noindent:      Option[Boolean] = None,
  _showlevel:     Option[Boolean] = None,
  _showtimes:     Option[Boolean] = None,
  _offset:        Option[Int]     = None,
  _specName:      Option[String]  = None,
  _sequential:    Option[Boolean] = None,
  _threadsNb:     Option[Int]     = None,
  _markdown:      Option[Boolean] = None,
  _debugMarkdown: Option[Boolean] = None,
  _diffs:         Option[Diffs]   = None,
  _fromSource:    Option[Boolean] = None,
  _commandLine:   Seq[String]     = Nil
 ) {
  def ex: String                = _ex.getOrElse(".*")
  def xonly: Boolean            = _xonly.getOrElse(false)
  def include: String           = _include.getOrElse("")
  def exclude: String           = _exclude.getOrElse("")
  def plan: Boolean             = _plan.getOrElse(false)
  def skipAll: Boolean          = _skipAll.getOrElse(false)
  def failtrace: Boolean        = _failtrace.getOrElse(false)
  def color: Boolean            = _color.getOrElse(true)
  def noindent: Boolean         = _noindent.getOrElse(false)
  def showlevel: Boolean        = _showlevel.getOrElse(false)
  def showtimes: Boolean        = _showtimes.getOrElse(false)
  def offset: Int               = _offset.getOrElse(0)
  def specName: String          = _specName.getOrElse(".*Spec")
  def sequential: Boolean       = _sequential.getOrElse(false)
  def threadsNb: Int            = _threadsNb.getOrElse(4)
  def markdown: Boolean         = _markdown.getOrElse(true)
  def debugMarkdown: Boolean    = _debugMarkdown.getOrElse(false)
  def diffs: Diffs              = _diffs.getOrElse(Diffs())
  def fromSource: Boolean       = _fromSource.getOrElse(true)
  def commandLine: Seq[String]  = _commandLine

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
    "commandLine"    -> (if (_commandLine.isEmpty) None else Some(_commandLine.mkString(", ")))
    ).flatMap(showArg).mkString(", ") + ") "
  }

  private def showArg(a: (String, Option[_])) = a._2.map(a._1 +" = "+_)
} 

private[specs2]  
object Arguments {
  
  /** @return new arguments from command-line arguments */
  def apply(implicit arguments: String*): Arguments = {
    extract(arguments)
  }
  private def extract(implicit arguments: Seq[String]): Arguments = {
    new Arguments (
       _ex            = value("ex", ".*"+(_:String)+".*"),
       _xonly         = bool("xonly"),
       _include       = value("include"),
       _exclude       = value("exclude"),
       _plan          = bool("plan"),
       _skipAll       = bool("skipall"),
       _failtrace     = bool("failtrace"),
       _color         = bool("color", "nocolor"),
       _noindent      = bool("noindent"),
       _showlevel     = bool("showlevel"),
       _showtimes     = bool("showtimes"),
       _offset        = int("offset"),
       _specName      = value("specname"),
       _sequential    = bool("sequential"),
       _threadsNb     = int("threadsNb"),
       _markdown      = bool("markdown", "nomarkdown"),
       _debugMarkdown = bool("debugmarkdown"),
       _fromSource    = bool("fromsource"),
       _commandLine   = arguments
    )
  }
  
  private def bool(name: String, mappedValue: Boolean = true)(implicit args: Seq[String]): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => mappedValue).orElse(boolSystemProperty(name, mappedValue))
  }
  private def boolSystemProperty(name: String, mappedValue: Boolean = true): Option[Boolean] = {
    SystemProperties.getProperty(name).map(a => mappedValue)
  }
  private def bool(name: String, negatedName: String)(implicit args: Seq[String]): Option[Boolean] = {
    bool(negatedName, false) orElse bool(name)
  }
  private def value(name: String, f: String => String)(implicit args: Seq[String]): Option[String] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(s => f(s._2)).orElse(valueSystemProperty(name, f))
  }
  private def valueSystemProperty(name: String, f: String => String): Option[String] = {
    SystemProperties.getProperty(name).map(o => f(o.toString))
  }
  private def value(name: String)(implicit args: Seq[String]): Option[String] = value(name, identity _)
  private def int(name: String)(implicit args: Seq[String]): Option[Int] = {
    tryo(value(name)(args).map(_.toInt).get)
  }

  implicit def ArgumentsMonoid: Monoid[Arguments] = new Monoid[Arguments] {
    def append(a1: Arguments, a2: =>Arguments) = a1 overrideWith a2
    val zero = Arguments()
  }
}

/**
 * The Diffs class holds all the required parameters to show differences between 2 strings
 */
case class Diffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, shortenSize: Int = 5, full: Boolean = false) {
  def show(expected: String, actual: String): Boolean = show && Seq(expected, actual).exists(_.size >= triggerSize)
}

