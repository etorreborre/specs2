package org.specs2
package main

import control._

/**
 * This trait provides shortcuts to create Arguments instances
 */
trait ArgumentsArgs extends ArgProperties {
  /** shorthand method to create an Arguments object */
  def args(
    ex:            ArgProperty[String]            = ArgProperty[String](),
    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    include:       ArgProperty[String]            = ArgProperty[String](),
    exclude:       ArgProperty[String]            = ArgProperty[String](),
    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    failtrace:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
    color:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    noindent:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    showlevel:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
    showtimes:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
    offset:        ArgProperty[Int]               = ArgProperty[Int](),
    specName:      ArgProperty[String]            = ArgProperty[String](),
    sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    threadsNb:     ArgProperty[Int]               = ArgProperty[Int](),
    markdown:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
    debugMarkdown: ArgProperty[Boolean]           = ArgProperty[Boolean](),
    diffs:         ArgProperty[Diffs]             = ArgProperty[Diffs](),
    fromSource:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    traceFilter:   ArgProperty[StackTraceFilter]  = ArgProperty[StackTraceFilter](),
    commandLine:   Seq[String]                    = Nil
  ) = new Arguments(
     ex.toOption.map(".*"+_+".*"),
     xonly.toOption,
     include.toOption,
     exclude.toOption,
     plan.toOption,
     skipAll.toOption,
     stopOnFail.toOption,
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
     debugMarkdown.toOption,
     diffs.toOption,
     fromSource.toOption,
     traceFilter.toOption,
     commandLine
  )
}
object ArgumentsArgs extends ArgumentsArgs

private[specs2]
trait ArgProperties {
  implicit def anyToArgProperty[T](t: =>T): ArgProperty[T] = ArgProperty(Property(t))
}
private[specs2]
object ArgProperties extends ArgProperties

private[specs2]
case class ArgProperty[T](p: Property[T] = Property[T]()) {
  def toOption: Option[T] = p.toOption
}