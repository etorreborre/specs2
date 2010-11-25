package org.specs2
package reporter

import main.Arguments

/**
 * This traits provides specialised print methods for message representing
 * different types of results. They can be overridden to define a more specific behaviour.
 */
trait ResultOutput {

  def printSuccess(message: String)(implicit args: Arguments)
  def printError(message: String)(implicit args: Arguments)
  def printSkipped(message: String)(implicit args: Arguments)
  def printPending(message: String)(implicit args: Arguments)
  /** print some text, splitting it on several lines */
  def printMessage(message: String)(implicit args: Arguments)
  def printLines(message: String)(implicit args: Arguments)
  /** print one line */
  def printLine(message: String)(implicit args: Arguments)
}
