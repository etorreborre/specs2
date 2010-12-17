package org.specs2
package main

import control.Exceptions._

private[specs2]  
case class Arguments (
  _ex:            Option[String]  = None,
  _xonly:         Option[Boolean] = None,
  _plan:          Option[Boolean] = None,
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
  _debugMarkdown: Option[Boolean] = None
) {
  def ex: String                = _ex.getOrElse(".*")
  def xonly: Boolean            = _xonly.getOrElse(false)
  def plan: Boolean             = _plan.getOrElse(false)
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

  /** @alias for overrideWith */
  def <|(other: Arguments) = overrideWith(other)
  
  /**
   * @return a new Argumentns object where the values of this are overriden with the values of other if defined
   */
  def overrideWith(other: Arguments): Arguments = {
    new Arguments(
      other._ex              .orElse(_ex),
      other._xonly           .orElse(_xonly),
      other._plan            .orElse(_plan),
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
      other._debugMarkdown   .orElse(_debugMarkdown)
    )
  }
  override def toString = {
    "Arguments("      +
    List(
    "ex"             -> _ex           ,
    "xonly"          -> _xonly        ,
    "plan"           -> _plan         ,
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
    "debugMarkdown"  -> _debugMarkdown
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
       _xonly         = bool("xonly"),
       _plan          = bool("plan"),
       _failtrace     = bool("failtrace"),
       _color         = bool("nocolor", false) orElse bool("color"),
       _noindent      = bool("noindent"),
       _showlevel     = bool("showlevel"),
       _showtimes     = bool("showtimes"),
       _offset        = int("offset"),
       _specName      = value("specName"),
       _sequential    = bool("sequential"),
       _threadsNb     = int("threadsNb"),
       _markdown      = bool("nomarkdown", false) orElse bool("markdown"),
       _debugMarkdown = bool("debugmarkdown")
    )
  }
  
  private def bool(name: String, mappedValue: Boolean = true)(implicit args: Seq[String]): Option[Boolean] = {
    args.find(_.toLowerCase.contains(name.toLowerCase)).map(a => mappedValue)
  }
  private def value(name: String)(implicit args: Seq[String]): Option[String] = {
    args.zip(args.drop(1)).find(_._1.toLowerCase.contains(name.toLowerCase)).map(_._2)
  }
  private def int(name: String)(implicit args: Seq[String]): Option[Int] = {
    tryo(value(name)(args).map(_.toInt).get)
  }
}

import control.Property
private[specs2]
trait ArgumentsArgs extends control.Properties {
  /** shorthand method to create an Arguments object */
  def args(  
    ex:            Property[String]   = Property[String](),
    xonly:         Property[Boolean]  = Property[Boolean](),
    plan:          Property[Boolean]  = Property[Boolean](),
    failtrace:     Property[Boolean]  = Property[Boolean](),
    color:         Property[Boolean]  = Property[Boolean](),
    noindent:      Property[Boolean]  = Property[Boolean](),
    showlevel:     Property[Boolean]  = Property[Boolean](),
    showtimes:     Property[Boolean]  = Property[Boolean](),
    offset:        Property[Int]      = Property[Int](),
    specName:      Property[String]   = Property[String](),
    sequential:    Property[Boolean]  = Property[Boolean](),
    threadsNb:     Property[Int]      = Property[Int](),
    markdown:      Property[Boolean]  = Property[Boolean](),
    debugMarkdown: Property[Boolean]  = Property[Boolean]()
  ) = new Arguments(
     ex.map(".*"+_+".*").toOption, 
     xonly.toOption, 
     plan.toOption, 
     failtrace.toOption, 
     color.toOption, 
     noindent.toOption, 
     showlevel.toOption, 
     showtimes.toOption,
     offset.toOption,
     specName.toOption, 
     sequential.toOption, 
     threadsNb.toOption,
     markdown.toOption,
     debugMarkdown.toOption
  )
  /** 
   * @return arguments for a literate specification: no auto indent and a sequential
   *         execution
   */
  def literate: Arguments = args(noindent = true, sequential = true)
  /** 
   * @return arguments for a specification where examples must be executed sequentially
   */
  def sequential: Arguments = args(sequential = true)
  /**
   * shortcut to show only the text without any execution
   */
  def plan: Arguments = args(plan = true)
  /**
   * shortcut to show only the text without automatic indentation
   */
  def freetext: Arguments = plan <| args(noindent = true)
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = true)
  /**
   * shortcut to executed and print only some examples
   */
  def only(examples: String): Arguments = args(ex = examples)
}
private[specs2]
object ArgumentsArgs extends ArgumentsArgs

