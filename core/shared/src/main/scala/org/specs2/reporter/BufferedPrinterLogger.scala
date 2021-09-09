package org.specs2
package reporter

import text.AnsiColors
import text.Whitespace.*

/** This line logger tries to respect line breaks in the original text.
  *
  * So if the original text is: Hello world\nHow are you? and we call infoLog("Hello world\n"); infoLog("How are you?")
  *
  * Then there will be only 2 lines displayed and not 3 (2 for the first infoLog, 1 for the second one)
  */
trait BufferedPrinterLogger extends PrinterLogger:
  enum Log:
    case Info, Failure, Warn, Error

  def infoLog(msg: String): Unit = { add(msg); flushText(); logType = Log.Info }
  def errorLog(msg: String): Unit = { add(msg); flushText(); logType = Log.Error }
  def failureLog(msg: String): Unit = { add(msg); flushText(); logType = Log.Failure }
  def warnLog(msg: String): Unit = { add(msg); flushText(); logType = Log.Warn }
  def newline(): Unit = { flushText(force = true); logType = Log.Info; add("") }
  def close(): Unit = { flushText(force = true) }

  protected def infoLine(msg: String): Unit
  protected def errorLine(msg: String): Unit
  protected def failureLine(msg: String): Unit
  protected def warnLine(msg: String): Unit

  private val buffer = new StringBuilder
  private var logType: Log = Log.Info

  private def add(msg: String): Unit =
    buffer.append(msg)
  // for debugging
  // println("msg '"+msg.showWhitespaces+"'")
  // println("buffer '"+buffer.toString.showWhitespaces+"'")
  // println("logType "+logType)

  private def flushText(force: Boolean = false): Unit =
    if force then
      val lines = buffer.toString.split("\n", -1)
      buffer.clear
      lines.foreach(log)
    else
      val lines = buffer.toString.split("\n", -1)
      buffer.clear
      lines.dropRight(1).foreach(log)
      lines.lastOption match
        case Some("")    => ()
        case Some(other) => add(other)
        case _           => ()

  private def log(s: String) =
    logType match
      case Log.Info    => infoLine(s)
      case Log.Failure => failureLine(s)
      case Log.Error   => errorLine(s)
      case Log.Warn    => warnLine(s)
