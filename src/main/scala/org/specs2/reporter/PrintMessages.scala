package org.specs2
package reporter

import io._
import text.Plural._
import main.Arguments
import specification._

/**
 * This traits provides specialized print methods which can be overriden to define more
 * specific behavior
 */
trait PrintMessages extends Output {

  def printError(message: String) = printMessage(message)
  def printSuccess(message: String) = printMessage(message)
  def printSkipped(message: String) = printMessage(message)
  def printPending(message: String) = printMessage(message)
  
  /**
   * print some text, splitting it on several lines
   */
  def printMessage(message: String) = {
    val splitted = message.split("\n")
    if (splitted.size > 1) splitted.foreach(m => printLine(m))
    else printLine(message)
  }
  /**
   * print one line
   */
  def printLine(message: String) = println(message)
}
