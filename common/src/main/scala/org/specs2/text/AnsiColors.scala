package org.specs2
package text

import NotNullStrings._

/**
 * This trait provides AnsiColors codes for the OutputReporter
 * @see http://en.wikipedia.org/wiki/ANSI_escape_code
 */
trait AnsiColors {
  lazy val black   = AnsiColor("\u001b[30m")
  lazy val red     = AnsiColor("\u001b[31m")
  lazy val green   = AnsiColor("\u001b[32m")
  lazy val yellow  = AnsiColor("\u001b[33m")
  lazy val blue    = AnsiColor("\u001b[34m")
  lazy val magenta = AnsiColor("\u001b[35m")
  lazy val cyan    = AnsiColor("\u001b[36m")
  lazy val white   = AnsiColor("\u001b[37m")
    
  lazy val reset   = AnsiColor("\u001b[0m")
    
  lazy val all = Seq(black, red, green, yellow, blue, magenta, cyan, white, reset)

  /** @return a string with no color codes */
  def removeColors(s: String, doIt: Boolean = true): String = {
    if (doIt) all.foldLeft (s.notNull) { (res, cur) => res.replace(cur.color, "") }
    else      s.notNull
  }

  /**
   * @return a colored string (if args.color == true)
   * color markers are inserted at the beginning and end of each line so that newlines are preserved
   */
  def color(s: String, color: AnsiColor, doIt: Boolean = true) = {
    if (doIt) {
      val colored = s.foldLeft(color.color) { (res, cur) =>
        if (cur == '\n') res + reset + cur + color.color
        else             res + cur
      } + reset
      colored
    }
    else removeColors(s, true)
  }

  override def toString = all.map(_.color).mkString("AnsiColors(",",",")")
}

object AnsiColors extends AnsiColors

case class AnsiColor(color: String) extends AnyVal