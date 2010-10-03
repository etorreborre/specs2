package org.specs2
package reporter

/**
 * This object provides AnsiColors codes for the OutputReporter
 * @see http://en.wikipedia.org/wiki/ANSI_escape_code
 */
trait AnsiColors {
  val black   = "\033[30m"
  val red     = "\033[31m"
  val green   = "\033[32m"
  val yellow  = "\033[33m"
  val blue    = "\033[34m"
  val magenta = "\033[35m"
  val cyan    = "\033[36m"
  val white   = "\033[37m"

  val reset   = "\033[0m"
}
object AnsiColors extends AnsiColors
