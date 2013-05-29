package org.specs2
package mutable

import main._
import text._
import control.StackTraceFilter

/**
 * This trait provides shortcuts to create Arguments instances and adding them to the SpecificationStructure by mutating its
 * current content
 */
trait ArgumentsArgs extends main.ArgumentsArgs { this: FragmentsBuilder =>
  override lazy val args = new ArgumentsNamespaceMutable

  /** shorthand method to create an Arguments object */
  override def args(
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

    addArguments(super.args(
      ex,
      include,
      exclude,
      wasIssue,
      was,
      plan,
      skipAll,
      stopOnFail,
      stopOnSkip,
      sequential,
      isolated,
      xonly,
      showOnly,
      color))


  private[specs2] class ArgumentsNamespaceMutable extends ArgumentsNamespace{
    /** shorthand method to create an Arguments object */
    override def select(
      ex:            ArgProperty[String]            = ArgProperty[String](),
      include:       ArgProperty[String]            = ArgProperty[String](),
      exclude:       ArgProperty[String]            = ArgProperty[String](),
      wasIssue:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      was:           ArgProperty[String]            = ArgProperty[String](),
      specName:      ArgProperty[String]            = ArgProperty[String]()) = addArguments(super.select(
        ex,
        include,
        exclude,
        wasIssue,
        was,
        specName))

    /** shorthand method to create an Arguments object */
    override def execute(
      plan:          ArgProperty[Boolean]           = ArgProperty[Boolean](),
      skipAll:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnFail:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      stopOnSkip:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      sequential:    ArgProperty[Boolean]           = ArgProperty[Boolean](),
      isolated:      ArgProperty[Boolean]           = ArgProperty[Boolean](),
      threadsNb:     ArgProperty[Int]               = ArgProperty[Int]()
    ) = addArguments(super.execute(
        plan,
        skipAll,
        stopOnFail,
        stopOnSkip,
        sequential,
        isolated,
        threadsNb))

    /** shorthand method to create an Arguments object */
    override def store(
      reset:       ArgProperty[Boolean]           = ArgProperty[Boolean](),
      never:       ArgProperty[Boolean]           = ArgProperty[Boolean]()
    ) = addArguments(super.store(
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
      debugMarkdown:     ArgProperty[Boolean]           = ArgProperty[Boolean](),
      pegdownExtensions: ArgProperty[Int]               = ArgProperty[Int](),
      streaming:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      diffs:             ArgProperty[Diffs]             = ArgProperty[Diffs](),
      fromSource:        ArgProperty[Boolean]           = ArgProperty[Boolean](),
      traceFilter:       ArgProperty[StackTraceFilter]  = ArgProperty[StackTraceFilter](),
      checkUrls:         ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notoc:             ArgProperty[Boolean]           = ArgProperty[Boolean](),
      notifier:          ArgProperty[String]            = ArgProperty[String](),
      exporter:          ArgProperty[String]            = ArgProperty[String]()) = addArguments(super.report(
        xonly,
        showOnly,
        failtrace,
        color,
        colors,
        showtimes,
        offset,
        debugMarkdown,
        pegdownExtensions,
        streaming,
        diffs,
        fromSource,
        traceFilter,
        checkUrls,
        notoc,
        notifier,
        exporter))
  }

}
