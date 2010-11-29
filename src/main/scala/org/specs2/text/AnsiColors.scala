package org.specs2
package text

/**
 * This trait provides AnsiColors codes for the OutputReporter
 * @see http://en.wikipedia.org/wiki/ANSI_escape_code
 */
private[specs2]
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
    
  val all = Seq(black, red, green, yellow, blue, magenta, cyan, white)
	 
  /** @return a string with no color codes */
  def removeColors(s: String): String = {
	  all.foldLeft (s) { (res, cur) =>
	    res.replace(cur, "")
	  }
  }	  
  def color(s: String, color: String, doIt: Boolean = true) = {
    if (doIt) 
      s.split("\n").map(color + _ + reset).mkString("\n")
    else s
  }
}
private[specs2]
object AnsiColors extends AnsiColors
