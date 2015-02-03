package org.specs2
package specification
package dsl
package mutable

import main._
import control.StackTraceFilter
import text.Colors

/**
 * Create arguments in an acceptance specification
 */
trait ArgumentsDsl extends ArgumentsCreation with ArgProperties

/**
 * Methods with default Property values to create Arguments instances
 * Arguments are being added to the SpecificationStructure by mutating its
 * current content
 *
 */
trait ArgumentsCreation extends org.specs2.main.ArgumentsCreation with MutableArgumentsBuilder {
  override lazy val args: ArgumentsNamespaceMutable = new ArgumentsNamespaceMutable

  /** shorthand method to create an Arguments object */
  override def args(
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

    setArguments(super.args(
      ex,
      include,
      exclude,
      was,
      plan,
      skipAll,
      stopOnFail,
      stopOnSkip,
      sequential,
      asap,
      isolated,
      xonly,
      showOnly,
      color))


  private[specs2] class ArgumentsNamespaceMutable extends ArgumentsNamespace {
    /** shorthand method to create an Arguments object */
    override def select(
                         ex:            ArgProperty[String]            = ArgProperty[String](),
                         include:       ArgProperty[String]            = ArgProperty[String](),
                         exclude:       ArgProperty[String]            = ArgProperty[String](),
                         was:           ArgProperty[String]            = ArgProperty[String](),
                         selector:      ArgProperty[String]            = ArgProperty[String]()) = setArguments(super.select(
      ex,
      include,
      exclude,
      was,
      selector))

    /** shorthand method to create an Arguments object */
    override def execute(
                          plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          asap:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
                          threadsNb:     ArgProperty[Int]               = ArgProperty[Int](),
                          executor:      ArgProperty[String]            = ArgProperty[String]()
                          ) = setArguments(super.execute(
      plan,
      skipAll,
      stopOnFail,
      stopOnSkip,
      sequential,
      asap,
      isolated,
      threadsNb,
      executor))

    /** shorthand method to create an Arguments object */
    override def store(
                        reset:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
                        never:       ArgProperty[Boolean]           = ArgProperty[Boolean]()
                        ) = setArguments(super.store(
      reset,
      never))

    /** shorthand method to create an Arguments object */
    override def report(
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
                         exporter:          ArgProperty[String]            = ArgProperty[String]()) = setArguments(super.report(
      xonly,
      showOnly,
      failtrace,
      color,
      colors,
      showtimes,
      offset,
      diffs,
      traceFilter,
      checkUrls,
      notoc,
      notifier,
      exporter))
  }


}



