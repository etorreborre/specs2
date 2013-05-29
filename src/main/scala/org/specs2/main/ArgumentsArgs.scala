package org.specs2
package main

import control._
import text._

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
    wasIssue:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    was:           ArgProperty[String]            = ArgProperty[String](),
    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    showOnly:      ArgProperty[String]            = ArgProperty[String](),
    color:         ArgProperty[Boolean]           = ArgProperty[Boolean]()) =

     (new ArgumentsNamespace).select(
            ex         = ex,
            include    = include,
            exclude    = exclude,
            wasIssue   = wasIssue,
            was        = was)       <|
     (new ArgumentsNamespace).execute(
              plan       = plan,
              skipAll    = skipAll,
              stopOnFail = stopOnFail,
              stopOnSkip = stopOnSkip,
              sequential = sequential,
              isolated   = isolated) <|
     (new ArgumentsNamespace).report(
              xonly      = xonly,
              showOnly   = showOnly,
              color      = color)


  private[specs2] class ArgumentsNamespace {
    /** shorthand method to create an Arguments object */
    def select(
      ex:            ArgProperty[String]            = ArgProperty[String](),
      include:       ArgProperty[String]            = ArgProperty[String](),
      exclude:       ArgProperty[String]            = ArgProperty[String](),
      wasIssue:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      was:           ArgProperty[String]            = ArgProperty[String](),
      specName:      ArgProperty[String]            = ArgProperty[String]()) = new Arguments(
       select = Select(
              ex.toOption.map(".*"+_+".*"),
              include.toOption,
              exclude.toOption,
              wasIssue.toOption.map(v => if (v) "x!" else "x!+-o*").orElse(was.toOption),
              specName.toOption))

    /** shorthand method to create an Arguments object */
    def execute(
      plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
      skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      threadsNb:     ArgProperty[Int]               = ArgProperty[Int]()
    ) = new Arguments(
       execute = Execute(plan.toOption,
               skipAll.toOption,
               stopOnFail.toOption,
               stopOnSkip.toOption,
               sequential.toOption,
               isolated.toOption,
               threadsNb.toOption))

    /** shorthand method to create an Arguments object */
    def store(
      reset:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
      never:       ArgProperty[Boolean]           = ArgProperty[Boolean]()
    ) = new Arguments(
       store = Store(reset.toOption,
                     never.toOption))

    /** shorthand method to create an Arguments object */
    def report(
      xonly:             ArgProperty[Boolean]           = ArgProperty[Boolean](),
      showOnly:          ArgProperty[String]            = ArgProperty[String](),
      failtrace:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      color:             ArgProperty[Boolean]           = ArgProperty[Boolean](),
      colors:            ArgProperty[Colors]            = ArgProperty[Colors](),
      showtimes:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      offset:            ArgProperty[Int]               = ArgProperty[Int](),
      debugMarkdown:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
      pegdownExtensions: ArgProperty[Int]               = ArgProperty[Int](),
      streaming:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      diffs:             ArgProperty[Diffs]             = ArgProperty[Diffs](),
      fromSource:        ArgProperty[Boolean]           = ArgProperty[Boolean](),
      traceFilter:       ArgProperty[StackTraceFilter]  = ArgProperty[StackTraceFilter](),
      checkUrls:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notoc:             ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notifier:          ArgProperty[String]            = ArgProperty[String](),
      exporter:          ArgProperty[String]            = ArgProperty[String]()) = new Arguments(
       report = Report(xonly.toOption.map(v => if (v) "x!" else "x!+-o*").orElse(showOnly.toOption),
                       failtrace.toOption,
                       color.toOption,
                       colors.toOption,
                       showtimes.toOption,
                       offset.toOption,
                       debugMarkdown.toOption,
                       pegdownExtensions.toOption,
                       streaming.toOption,
                       diffs.toOption,
                       fromSource.toOption,
                       traceFilter.toOption,
                       checkUrls.toOption,
                       notoc.toOption,
                       notifier.toOption,
                       exporter.toOption))

  }

}

object ArgumentsArgs extends ArgumentsArgs

trait ArgProperties {
  implicit def anyToArgProperty[T](t: =>T): ArgProperty[T] = ArgProperty(Property(t))
}

/**
 * This trait can be used to deactivate the conversion of any value to an ArgsProperty
 */
trait NoArgProperties extends ArgProperties {
  override def anyToArgProperty[T](t: =>T): ArgProperty[T] = super.anyToArgProperty(t)
}

object ArgProperties extends ArgProperties

case class ArgProperty[T](private val aProperty: Property[T] = Property[T]()) {
  def toOption: Option[T] = aProperty.toOption
}