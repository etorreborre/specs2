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
trait BufferedLineLogger extends LineLogger {
  def infoLog(msg: String)   : Unit = { val rest = flushText(); add(rest+msg) }
  def errorLog(msg: String)  : Unit = { val rest = flushText(); errorLine(rest+msg)  }
  def failureLog(msg: String): Unit = { val rest = flushText(); failureLine(rest+msg) }
  def warnLog(msg: String)   : Unit = { val rest = flushText(); warnLine(rest+msg) }
  def newline()              : Unit = { infoLine(buffer.toString); buffer.clear }
  def close()                : Unit = { flushText(force = true); () }

  protected def infoLine(msg: String): Unit
  protected def errorLine(msg: String): Unit
  protected def failureLine(msg: String): Unit
  protected def warnLine(msg: String): Unit

  private val buffer = new StringBuilder
  private def add(msg: String): Unit = { buffer.append(msg); () }

  private def flushText(force: Boolean = false): String = {
    if (force) {
      if (!buffer.isEmpty) infoLine(buffer.toString)
      buffer.clear
      ""
    } else if (endsWith(buffer.toString, "\n")) {
      val lines = buffer.toString.split("\n")
      buffer.clear
      if (lines.size == 1) {
        infoLine(lines.mkString)
        ""
      }
      else {
        lines.dropRight(1).foreach(infoLine)
        lines.lastOption.getOrElse("")
      }
    } else ""
  }

  private def endsWith(message: String, string: String) = {
    val nocolor = AnsiColors.removeColors(message)
    nocolor.nonEmpty &&
      nocolor.reverse.
        dropWhile(_ == ' ').
        startsWith(string)
  }

}

