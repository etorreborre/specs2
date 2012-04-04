package org.specs2
package main

import control.{NoStackTraceFilter, IncludeExcludeStackTraceFilter, DefaultStackTraceFilter, Exceptions}
import Exceptions._
import text._

/**
 * This trait provides shortcuts for frequently used arguments
 */
trait ArgumentsShortcuts { this: ArgumentsArgs =>
  /**
   * @return arguments for a literate specification: no auto indent and a sequential
   *         execution
   */
  def literate: Arguments = sequential <| noindent
  /**
   * @return arguments for a specification where examples must be executed sequentially
   */
  def sequential: Arguments = args(sequential = true)
  /**
   * @return arguments for a specification where examples must be executed in their own specification
   */
  def isolated: Arguments = args(isolated = true)
  /**
   * shortcut to show only the text without any execution
   */
  def plan: Arguments = args(plan = true)
  /**
   * shortcut to skip all examples
   */
  def skipAll: Arguments = args(skipAll = true)
  /**
   * shortcut to skip all examples when a condition is true.
   * if the condition throws an exception, its stacktrace is *not* printed and
   * all the examples are skipped
   */
  def skipAllIf(condition: =>Boolean): Arguments = args(skipAll = tryo(condition).getOrElse(true): Boolean)
  /**
   * shortcut to stop after the first failure or error
   */
  def stopOnFail: Arguments = args(stopOnFail = true)
  /**
   * shortcut to stop after the first skipped result
   */
  def stopOnSkip: Arguments = args(stopOnSkip = true)
  /**
   * shortcut to avoid automatic indentation
   */
  def noindent: Arguments = args(noindent = true)
  /**
   * shortcut to avoid colored output
   */
  def nocolor: Arguments = args(color = false)
  /**
   * shortcut to set new Colors
   */
  def colors(c: Colors): Arguments = args.report(colors = c)
  /**
   * shortcut to not executing the text and avoid automatic indentation
   */
  def freetext: Arguments = plan <| noindent
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = true)
  /**
   * shortcut to print only some statuses
   */
  def showOnly(s: String): Arguments = args(showOnly = s)
  /**
   * shortcut to execute and print only some examples
   */
  def only(examples: String): Arguments = args(ex = examples)
  /**
   * shortcut to include only some tagged fragments
   */
  def include(tags: String): Arguments = args(include = tags)
  /**
   * shortcut to exclude some tagged fragments
   */
  def exclude(tags: String): Arguments = args(exclude = tags)
  /**
   * shortcut to include only previousy failed/errors examples
   */
  def wasIssue: Arguments = args(wasIssue = true)
  /**
   * shortcut to include only examples with some previous statuses
   */
  def was(s: String): Arguments = args(was = s)

  /**
   * shortcut to display the differences with some specific parameters
   */
  def diffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, diffRatio: Int = 30, shortenSize: Int = 5, full: Boolean = false): Arguments =
    args.report(diffs = SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full))
  /**
   * shortcut to display the example descriptions from the expectations ok messages
   */
  def descFromExpectations = args.report(fromSource = false)
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
  def fullStackTrace = args.report(traceFilter = NoStackTraceFilter)
}

object ArgumentsShortcuts extends ArgumentsShortcuts with ArgumentsArgs