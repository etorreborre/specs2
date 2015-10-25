package org.specs2
package main

import data.SeparatedTags

/**
 * Selection arguments
 */
case class Select(
                   _ex:            Option[String]           = None,
                   _times:         Option[String]           = None,
                   _include:       Option[String]           = None,
                   _exclude:       Option[String]           = None,
                   _was:           Option[String]           = None,
                   _selector:      Option[String]           = None) extends ShowArgs {

  import Arguments._

  def ex: String                    = _ex.getOrElse(".*")

  def times: Int                    = {
    val default: Int = 1

    _times flatMap { t =>
      scala.util.Try(t.toInt).toOption map {
        case i if i > 0 => i
        case _ => default  // non-positive value
      }
    } getOrElse default // non-numeric value, or value not provided
  }

  def include: String               = _include.getOrElse("")
  def exclude: String               = _exclude.getOrElse("")
  def keep(tags: String*)           = SeparatedTags(include, exclude).keep(tags)
  def contain(tags: String*)        = SeparatedTags(include, exclude).contain(tags)
  def hasFilter                     = Seq(_include, _exclude, _ex, _was).exists(_.isDefined)
  def was(s: String): Boolean       = hasFlags(s, _was)
  def wasIsDefined: Boolean         = _was.isDefined
  def selector                      = _selector.getOrElse("")

  def overrideWith(other: Select) = {
    new Select(
      other._ex              .orElse(_ex),
      other._times           .orElse(_times),
      other._include         .orElse(_include),
      other._exclude         .orElse(_exclude),
      other._was             .orElse(_was),
      other._selector        .orElse(_selector)
    )
  }

  override def toString = List(
    "ex"             -> _ex         ,
    "times"          -> _times      ,
    "include"        -> _include    ,
    "exclude"        -> _exclude    ,
    "was"            -> _was        ,
    "selector"       -> _selector  ).flatMap(showArg).mkString("Select(", ", ", ")")
}

object Select extends Extract {
  def extract(implicit arguments: Seq[String], systemProperties: SystemProperties): Select = {
    new Select (
      _ex            = value("ex", ".*"+(_:String)+".*"),
      _times         = value("times"),
      _include       = value("include"),
      _exclude       = value("exclude"),
      _was           = value("was"),
      _selector      = value("selector")
    )
  }
  val allValueNames = Seq("ex", "times", "include", "exclude", "was", "selector")
}
