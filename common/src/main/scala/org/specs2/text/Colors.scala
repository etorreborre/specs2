package org.specs2
package text

/**
 * This trait defines the colors which can be used to output text on the console
 *
 * The textColor ... statsColor define the AnsiColor to use
 * The text ... stats methods taking a String as a parameter are inserting control characters to change the string color
 */
trait Colors {
  def textColor   : AnsiColor
  def successColor: AnsiColor
  def failureColor: AnsiColor
  def errorColor  : AnsiColor
  def pendingColor: AnsiColor
  def skippedColor: AnsiColor
  def statsColor  : AnsiColor

  def text   (s: String, doIt: Boolean = true): String
  def success(s: String, doIt: Boolean = true): String
  def failure(s: String, doIt: Boolean = true): String
  def error  (s: String, doIt: Boolean = true): String
  def pending(s: String, doIt: Boolean = true): String
  def skipped(s: String, doIt: Boolean = true): String
  def stats  (s: String, doIt: Boolean = true): String

  def removeColors(s: String): String
}

