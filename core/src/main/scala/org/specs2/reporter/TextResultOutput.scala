package org.specs2
package reporter

import io.ConsoleOutput
import main.Arguments
import execute.Result
import specification.Stats
import text.Trim._

/**
 * Implementation of the ResultOutput trait as Text
 */
class TextResultOutput extends LineLoggerOutput with ConsoleOutput {

  def infoLog(msg: String)    = println(msg)
  def failureLog(msg: String) = println(msg)
  def errorLog(msg: String)   = println(msg)

}

