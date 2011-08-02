package org.specs2
package main

import control._
import reporter.Colors

/**
 * This trait provides shortcuts to create Arguments instances
 */
trait ArgumentsArgs extends ArgProperties {

  lazy val args = new ArgumentsNamespace

  /** shorthand method to create an Arguments object */
  def args(
    ex:            ArgProperty[String]            = ArgProperty[String](),
    include:       ArgProperty[String]            = ArgProperty[String](),
    exclude:       ArgProperty[String]            = ArgProperty[String](),
    failedOnly:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    color:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    noindent:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    markdown:      ArgProperty[Boolean]           = ArgProperty[Boolean]()) = new Arguments(

     select = Select(_ex      = ex.toOption.map(".*"+_+".*"),
                     _include = include.toOption,
                     _exclude = exclude.toOption,
                     _failedOnly = failedOnly.toOption),

     execute = Execute(_plan       = plan.toOption,
                       _skipAll    = skipAll.toOption,
                       _stopOnFail = stopOnFail.toOption,
                       _sequential = sequential.toOption),

     report = Report(_xonly   = xonly.toOption,
                     _color    = color.toOption,
                     _noindent = noindent.toOption,
                     _markdown = markdown.toOption))


  private[specs2] class ArgumentsNamespace {
    /** shorthand method to create an Arguments object */
    def select(
      ex:            ArgProperty[String]            = ArgProperty[String](),
      include:       ArgProperty[String]            = ArgProperty[String](),
      exclude:       ArgProperty[String]            = ArgProperty[String](),
      failedOnly:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      specName:      ArgProperty[String]            = ArgProperty[String]()) = new Arguments(
       select = Select(ex.toOption.map(".*"+_+".*"),
              include.toOption,
              exclude.toOption,
              failedOnly.toOption,
              specName.toOption))

    /** shorthand method to create an Arguments object */
    def execute(
      plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
      skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      threadsNb:     ArgProperty[Int]               = ArgProperty[Int]()
    ) = new Arguments(
       execute = Execute(plan.toOption,
               skipAll.toOption,
               stopOnFail.toOption,
               sequential.toOption,
               threadsNb.toOption))

    /** shorthand method to create an Arguments object */
    def report(
      xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      failtrace:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
      color:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      colors:        ArgProperty[Colors]            = ArgProperty[Colors](),
      noindent:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      showtimes:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
      offset:        ArgProperty[Int]               = ArgProperty[Int](),
      markdown:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      debugMarkdown: ArgProperty[Boolean]           = ArgProperty[Boolean](),
      diffs:         ArgProperty[Diffs]             = ArgProperty[Diffs](),
      fromSource:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      traceFilter:   ArgProperty[StackTraceFilter]  = ArgProperty[StackTraceFilter]()) = new Arguments(
       report = Report(xonly.toOption,
                       failtrace.toOption,
                       color.toOption,
                       colors.toOption,
                       noindent.toOption,
                       showtimes.toOption,
                       offset.toOption,
                       markdown.toOption,
                       debugMarkdown.toOption,
                       diffs.toOption,
                       fromSource.toOption,
                       traceFilter.toOption))

  }

}

object ArgumentsArgs extends ArgumentsArgs

trait ArgProperties {
  implicit def anyToArgProperty[T](t: =>T): ArgProperty[T] = ArgProperty(Property(t))
}
object ArgProperties extends ArgProperties

case class ArgProperty[T](p: Property[T] = Property[T]()) {
  def toOption: Option[T] = p.toOption
}