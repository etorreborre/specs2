package org.specs2
package main

import control.{NoStackTraceFilter, IncludeExcludeStackTraceFilter, DefaultStackTraceFilter}

/**
 * This trait provides shortcuts for frequently used arguments
 */
trait ArgumentsShortcuts { this: ArgumentsArgs =>
  /**
   * @return arguments for a literate specification: no auto indent and a sequential
   *         execution
   */
  def literate: Arguments = args(noindent = true, sequential = true)
  /**
   * @return arguments for a specification where examples must be executed sequentially
   */
  def sequential: Arguments = args(sequential = true)
  /**
   * shortcut to show only the text without any execution
   */
  def plan: Arguments = args(plan = true)
  /**
   * shortcut to skip all examples
   */
  def skipAll: Arguments = args(skipAll = true)
  /**
   * shortcut to avoid automatic indentation
   */
  def noindent: Arguments = args(noindent = true)
  /**
   * shortcut to not executing the text and avoid automatic indentation
   */
  def freetext: Arguments = plan <| noindent
  /**
   * shortcut to print only failures and errors
   */
  def xonly: Arguments = args(xonly = true)
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
   * shortcut to display the differences with some specific parameters
   */
  def diffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, diffRatio: Int = 30, shortenSize: Int = 5, full: Boolean = false): Arguments =
    args(diffs = SmartDiffs(show, separators, triggerSize, shortenSize, diffRatio, full))
  /**
   * shortcut to display the example descriptions from the expectations ok messages
   */
  def descFromExpectations = args(fromSource = false)
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
  def fullStackTrace = args(traceFilter = NoStackTraceFilter)
}
