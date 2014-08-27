package org.specs2
package main

import control.{NoStackTraceFilter, IncludeExcludeStackTraceFilter, DefaultStackTraceFilter, Exceptions}
import Exceptions._
import text._

/**
 * This trait provides shortcuts for frequently used arguments
 */
trait ArgumentsShortcuts { this: ArgumentsCreation =>
  /**
   * @return arguments for a specification where examples must be executed sequentially
   */
  def sequential: Arguments = args(sequential = ArgProperty(true))
  /**
   * @return arguments for a specification where examples must be executed in their own specification
   */
  def isolated: Arguments = args(isolated = ArgProperty(true))
  /**
   * shortcut to show only the text without any execution
   */
  def plan: Arguments = args(plan = ArgProperty(true))
  /**
   * shortcut to skip all examples
   */
  def skipAll: Arguments = args(skipAll = ArgProperty(true))
  /**
   * shortcut to skip all examples when a condition is true.
   * if the condition throws an exception, its stacktrace is *not* printed and
   * all the examples are skipped
   */
  def skipAllIf(condition: =>Boolean): Arguments = args(skipAll = ArgProperty(tryo(condition).getOrElse(true): Boolean))
  /**
   * shortcut to skip all examples when a condition is false.
   */
  def skipAllUnless(condition: =>Boolean): Arguments = skipAllIf(!condition)
  /**
   * shortcut to stop after the first failure or error
   */
  def stopOnFail: Arguments = args(stopOnFail = ArgProperty(true))
  /**
   * shortcut to stop after the first skipped result
   */
  def stopOnSkip: Arguments = args(stopOnSkip = ArgProperty(true))
  /**
   * shortcut to avoid colored output
   */
  def nocolor: Arguments = args(color = ArgProperty(false))
  /**
   * shortcut to set new Colors
   */
  def colors(c: Colors): Arguments = args.report(colors = ArgProperty(c))
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = ArgProperty(true))
  /**
   * shortcut to print only some statuses
   */
  def showOnly(s: String): Arguments = args(showOnly = ArgProperty(s))
  /**
   * shortcut to execute and print only some examples
   */
  def only(examples: String): Arguments = args(ex = ArgProperty(examples))
  /**
   * shortcut to include only some tagged fragments
   */
  def include(tags: String): Arguments = args(include = ArgProperty(tags))
  /**
   * shortcut to exclude some tagged fragments
   */
  def exclude(tags: String): Arguments = args(exclude = ArgProperty(tags))
  /**
   * shortcut to include only examples with some previous statuses
   */
  def was(s: String): Arguments = args(was = ArgProperty(s))

  /**
   * shortcut to display the differences with some specific parameters
   */
  def diffs(show: Boolean      = true,
            separators: String = "[]",
            triggerSize: Int   = 20,
            diffRatio: Int     = 30,
            shortenSize: Int   = 5,
            full: Boolean      = false): Arguments =
    args.report(diffs = ArgProperty(SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full)))
  /**
   * shortcut to create a stackTrace filter to include only some elements
   */
  def includeTrace(patterns: String*) = new IncludeExcludeStackTraceFilter(patterns.toSeq, Seq[String]())
  /**
   * shortcut to add include trace patterns
   */
  def includeAlsoTrace(patterns: String*) = DefaultStackTraceFilter.includeAlso(patterns:_*)
  /**
   * shortcut to create a stackTrace filter to exclude only some elements
   */
  def excludeTrace(patterns: String*) = new IncludeExcludeStackTraceFilter(Seq[String](), patterns.toSeq)
  /**
   * shortcut to add exclude trace patterns
   */
  def excludeAlsoTrace(patterns: String*) = DefaultStackTraceFilter.excludeAlso(patterns:_*)
  /**
   * shortcut to filter nothing
   */
  def fullStackTrace = args.report(traceFilter = ArgProperty(NoStackTraceFilter))
}

object ArgumentsShortcuts extends ArgumentsShortcuts with ArgumentsArgs