package org.specs2
package reporter

import io.{StringOutput, ConsoleOutput}
import text.AnsiColors

/**
 * Logger with info, failure, error where each new message is displayed on a new line
 */
trait PrinterLogger:
  def infoLog(msg: String): Unit
  def failureLog(msg: String): Unit
  def errorLog(msg: String): Unit
  def warnLog(msg: String): Unit
  def newline(): Unit
  def close(): Unit

object PrinterLogger:

  /** line logger implementation for the console */
  lazy val consolePrinterLogger = new BufferedPrinterLogger with ConsoleOutput {
    protected def infoLine(msg: String)    = println("[info] " + msg)
    protected def errorLine(msg: String)   = println("[error] " + msg)
    protected def failureLine(msg: String) = println("[error] " + msg)
    protected def warnLine(msg: String)    = println("[warn] " + msg)
    override def toString = "consolePrinterLogger"
  }

  lazy val NoPrinterLogger = new PrinterLogger {
    def infoLog(msg: String)    = ()
    def failureLog(msg: String) = ()
    def errorLog(msg: String)   = ()
    def warnLog(msg: String)    = ()
    def close()                 = ()
    def newline()               = ()
    override def toString = "NoPrinterLogger"
  }

  /** this logger can be used for tests */
  def stringPrinterLogger = new BufferedPrinterLogger with StringOutput {
    def infoLine(msg: String)    = msg.split("\n").foreach(m => append("[info] " + m))
    def errorLine(msg: String)   = msg.split("\n").foreach(m => append("[error] " + m))
    def failureLine(msg: String) = msg.split("\n").foreach(m => append("[error] " + m))
    def warnLine(msg: String)    = msg.split("\n").foreach(m => append("[warn] " + m))

    override def append(m: String) = super.append(AnsiColors.removeColors(m))
    override def toString = "stringPrinterLogger"
  }
