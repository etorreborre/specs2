package org.specs2
package text

import NotNullStrings._

/**
 * This trait provides AnsiColors codes for the OutputReporter
 * @see http://en.wikipedia.org/wiki/ANSI_escape_code
 */
trait AnsiColors {
  lazy val black   = "\u001b[30m"
  lazy val red     = "\u001b[31m"
  lazy val green   = "\u001b[32m"
  lazy val yellow  = "\u001b[33m"
  lazy val blue    = "\u001b[34m"
  lazy val magenta = "\u001b[35m"
  lazy val cyan    = "\u001b[36m"
  lazy val white   = "\u001b[37m"
    
  lazy val reset   = "\u001b[0m"
    
  lazy val all = Seq(black, red, green, yellow, blue, magenta, cyan, white, reset)

  /** @return a string with no color codes */
  def removeColors(s: String, doIt: Boolean = true): String = {
    if (doIt) all.foldLeft (s.notNull) { (res, cur) => res.replace(cur, "") }
    else      s.notNull
  }

  /**
   * @return a colored string (if args.color == true)
   * color markers are inserted at the beginning and end of each line so that newlines are preserved
   */
  def color(s: String, color: String, doIt: Boolean = true) = {
    if (doIt) {
      val colored = s.foldLeft(color) { (res, cur) =>
        if (cur == '\n') res + reset + cur + color
        else             res + cur
      } + reset
      colored
    }
    else      removeColors(s, true)
  }

  override def toString = all.mkString("AnsiColors(",",",")")
}
object AnsiColors extends AnsiColors

