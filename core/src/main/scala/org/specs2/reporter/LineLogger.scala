package org.specs2
package reporter

import io.{StringOutput, ConsoleOutput}
import text.AnsiColors

/**
 * Logger with info, failure, error where each new message is displayed on a new line
 */
trait LineLogger {
  def infoLog(msg: String)
  def failureLog(msg: String)
  def errorLog(msg: String)
  def warnLog(msg: String)
  def newline()
  def close()
}

object LineLogger {

  /** line logger implementation for the console */
  lazy val consoleLogger = new BufferedLineLogger with ConsoleOutput {
    protected def infoLine(msg: String)    = println("[info] " + msg)
    protected def errorLine(msg: String)   = println("[error] " + msg)
    protected def failureLine(msg: String) = println("[error] " + msg)
    protected def warnLine(msg: String)    = println("[warn] " + msg)
    override def toString = "consoleLogger"
  }

  lazy val NoLineLogger = new LineLogger {
    def infoLog(msg: String)    = ()
    def failureLog(msg: String) = ()
    def errorLog(msg: String)   = ()
    def warnLog(msg: String)    = ()
    def close()                 = ()
    def newline()               = ()
    override def toString = "NoLineLogger"
  }

  /** this logger can be used for tests */
  def stringLogger = new BufferedLineLogger with StringOutput {
    def infoLine(msg: String)    = msg.split("\n").foreach(m => append("[info] " + m))
    def errorLine(msg: String)   = msg.split("\n").foreach(m => append("[error] " + m))
    def failureLine(msg: String) = msg.split("\n").foreach(m => append("[error] " + m))
    def warnLine(msg: String)    = msg.split("\n").foreach(m => append("[warn] " + m))

    override def append(m: String) = super.append(AnsiColors.removeColors(m))
    override def toString = "stringLogger"
  }
}



