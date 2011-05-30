package org.specs2
package mutable

import main._
import text._
import specification.Fragments
import control.{StackTraceFilter, Property}
import reporter.Colors

/**
 * This trait provides shortcuts to create Arguments instances and adding them to the SpecificationStructure by mutating its
 * current content
 */
trait ArgumentsArgs extends main.ArgumentsArgs { this: FragmentsBuilder =>
  /** shorthand method to create an Arguments object */
  override def args(
    ex:            ArgProperty[String]            = ArgProperty[String](),
    xonly:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    include:       ArgProperty[String]            = ArgProperty[String](),
    exclude:       ArgProperty[String]            = ArgProperty[String](),
    plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
    skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
    stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
    failtrace:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
    color:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
    colors:        ArgProperty[Colors]            = ArgProperty[Colors](),
    noindent:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
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
  ) = {
    addArguments(super.args(
      ex,
      xonly,
      include,
      exclude,
      plan,
      skipAll,
      stopOnFail,
      failtrace,
      color,
      colors,
      noindent,
      showtimes,
      offset,
      specName,
      sequential,
      threadsNb,
      markdown,
      debugMarkdown,
      diffs,
      fromSource,
      traceFilter,
      commandLine))
  }

}
