package org.specs2
package main

import control._
import text._

/**
 * Methods with default Property values to create Arguments instances
 * 
 * There is an implicit conversion from (=> T) to Property[T] to allow the direct passing of parameters 
 */
trait ArgumentsArgs extends ArgumentsCreation with ArgProperties

/**
 * Methods with default Property values to create Arguments instances
 */
trait ArgumentsCreation {

  lazy val args: ArgumentsNamespace = new ArgumentsNamespace

  /** shorthand method to create an Arguments object */
  def args(
    ex:            ArgProperty[String]            = ArgProperty[String](),
    include:       ArgProperty[String]            = ArgProperty[String](),
    exclude:       ArgProperty[String]            = ArgProperty[String](),
    was:           ArgProperty[String]            = ArgProperty[String](),
    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    asap:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    showOnly:      ArgProperty[String]            = ArgProperty[String](),
    color:         ArgProperty[Boolean]           = ArgProperty[Boolean]()): Arguments =

     (new ArgumentsNamespace).select(
            ex         = ex,
            include    = include,
            exclude    = exclude,
            was        = was)       <|
     (new ArgumentsNamespace).execute(
              plan       = plan,
              skipAll    = skipAll,
              stopOnFail = stopOnFail,
              stopOnSkip = stopOnSkip,
              sequential = sequential,
              asap = asap,
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
      was:           ArgProperty[String]            = ArgProperty[String](),
      selector:      ArgProperty[String]            = ArgProperty[String]()) = new Arguments(
       select = Select(
              ex.toOption.map(".*"+_+".*"),
              include.toOption,
              exclude.toOption,
              was.toOption,
              selector.toOption))

    /** shorthand method to create an Arguments object */
    def execute(
      plan:               ArgProperty[Boolean]           = ArgProperty[Boolean](),
      skipAll:            ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnFail:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnSkip:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      sequential:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      asap:               ArgProperty[Boolean]           = ArgProperty[Boolean](),
      isolated:           ArgProperty[Boolean]           = ArgProperty[Boolean](),
      threadsNb:          ArgProperty[Int]               = ArgProperty[Int](),
      scheduledThreadsNb: ArgProperty[Int]               = ArgProperty[Int](),
      timeFactor:         ArgProperty[Int]               = ArgProperty[Int](),
      executor:           ArgProperty[String]            = ArgProperty[String]()
    ) = new Arguments(
       execute = Execute(plan.toOption,
               skipAll.toOption,
               stopOnFail.toOption,
               stopOnSkip.toOption,
               sequential.toOption,
               asap.toOption,
               isolated.toOption,
               threadsNb.toOption,
               scheduledThreadsNb.toOption,
               timeFactor.toOption,
               executor.toOption))

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
      diffs:             ArgProperty[Diffs]             = ArgProperty[Diffs](),
      traceFilter:       ArgProperty[StackTraceFilter]  = ArgProperty[StackTraceFilter](),
      checkUrls:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notoc:             ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notifier:          ArgProperty[String]            = ArgProperty[String](),
      exporter:          ArgProperty[String]            = ArgProperty[String]()) = new Arguments(
       report = Report(xonly.toOption.map(v => if (v) Report.xonlyFlags else Report.allFlags).orElse(showOnly.toOption),
                       failtrace.toOption,
                       color.toOption,
                       colors.toOption,
                       showtimes.toOption,
                       offset.toOption,
                       diffs.toOption,
                       traceFilter.toOption,
                       checkUrls.toOption,
                       notoc.toOption,
                       notifier.toOption,
                       exporter.toOption))

  }

}

object ArgumentsArgs extends ArgumentsArgs

trait ArgProperties {
  implicit def anyToArgProperty[T](t: =>T): ArgProperty[T] = ArgProperty(t)
}

/**
 * This trait can be used to deactivate the conversion of any value to an ArgsProperty
 */
trait NoArgProperties extends ArgProperties {
  override def anyToArgProperty[T](t: =>T): ArgProperty[T] = super.anyToArgProperty(t)
}

object ArgProperties extends ArgProperties

class ArgProperty[T](aProperty: Property[T] = Property[T]()) {
  def toOption: Option[T] = aProperty.toOption
}

object ArgProperty {
  def apply[T](): ArgProperty[T] =
    new ArgProperty()

  def apply[T](t: => T): ArgProperty[T] =
    new ArgProperty(Property(t))
}
