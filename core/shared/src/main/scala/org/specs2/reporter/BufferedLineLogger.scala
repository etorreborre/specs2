package org.specs2
package reporter

import text.AnsiColors

/**
 * This line logger tries to respect line breaks in the original text.
 *
 * So if the original text is: Hello world\nHow are you?
 * and we call infoLog("Hello world\n"); infoLog("How are you?")
 *
 * Then there will be only 2 lines displayed and not 3 (2 for the first infoLog, 1 for the second one)
 */
trait BufferedPrinterLogger extends PrinterLogger:
  def infoLog(msg: String)   : Unit = { add(msg); flushText() }
  def errorLog(msg: String)  : Unit = { flushText(); errorLine(msg)  }
  def failureLog(msg: String): Unit = { flushText(); failureLine(msg) }
  def warnLog(msg: String)   : Unit = { flushText(); warnLine(msg) }
  def newline()              : Unit = { infoLine(buffer.toString); buffer.clear }
  def close()                : Unit = { flushText(force = true); () }

  protected def infoLine(msg: String): Unit
  protected def errorLine(msg: String): Unit
  protected def failureLine(msg: String): Unit
  protected def warnLine(msg: String): Unit

  private val buffer = new StringBuilder

  private def add(msg: String): Unit =
    buffer.append(msg)

  private def flushText(force: Boolean = false): Unit =
    if force then
      val lines = buffer.toString.split("\n")
      buffer.clear
      lines.foreach(infoLine)
    else
      val lines = buffer.toString.split("\n", -1)
      if lines.size > 1 then
        buffer.clear
        lines.dropRight(1).foreach(infoLine)
        add(lines.lastOption.getOrElse(""))

  private def endsWith(message: String, string: String) =
    val nocolor = AnsiColors.removeColors(message)
    nocolor.nonEmpty &&
      nocolor.reverse.
        dropWhile(_ == ' ').
        startsWith(string)
