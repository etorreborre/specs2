package org.specs2
package text

import control.Exceptions.*
import java.util.regex.Pattern
import util.matching.Regex
import Trim.*

/** This trait provides utility functions for working with regexes
  */
trait Regexes:

  extension (s: String)
    /** matchesSafely a pattern p. If p cannot be compiled, then it is quoted if the string s is enclosed with
      * characters, they can be excluded before the quotation is done
      */
    infix def matchesSafely(p: String, enclosing: String = ""): Boolean =
      val pattern = tryOrElse(Pattern.compile(enclosing + p + enclosing))(
        Pattern.compile(enclosing + Pattern.quote(p.trimEnclosing(enclosing)) + enclosing)
      )
      pattern.matcher(s).matches

  /** @return a regular expression String matching 's' inside another string, possibly multi-string */
  def regexPart: String =
    s"(?s).*$s.*"

  extension (r: Regex) def matches(s: String): Boolean = r.pattern.matcher(s).matches

  extension (p: Pattern)
    def regexPart: String =
      p.toString.regexPart

object Regexes extends Regexes
