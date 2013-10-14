package org.specs2
package text

/**
 * This trait defines the colors which must be used to output text on the console
 */
trait Colors {
  def textColor   : String
  def successColor: String
  def failureColor: String
  def errorColor  : String
  def pendingColor: String
  def skippedColor: String
  def statsColor  : String

  def text   (s: String, doIt: Boolean = true): String
  def success(s: String, doIt: Boolean = true): String
  def failure(s: String, doIt: Boolean = true): String
  def error  (s: String, doIt: Boolean = true): String
  def pending(s: String, doIt: Boolean = true): String
  def skipped(s: String, doIt: Boolean = true): String
  def stats  (s: String, doIt: Boolean = true): String

  def removeColors(s: String): String
}

