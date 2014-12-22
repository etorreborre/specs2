package org.specs2
package text

import control.Exceptions._
import java.util.regex.Pattern
import util.matching.Regex
import Trim._

/**
 * This trait provides utility functions for working with regexes
 */
trait Regexes {

  implicit class Regexed(s: String) {

    /**
     * matchesSafely a pattern p. If p cannot be compiled, then it is quoted
     * if the string s is enclosed with characters, they can be excluded before the quotation is done
     */
    def matchesSafely(p: String, enclosing: String = ""): Boolean = {
      val pattern = tryOrElse(Pattern.compile(p))(Pattern.compile(enclosing+Pattern.quote(p.trimEnclosing(enclosing))+enclosing))
      pattern.matcher(s.removeAll("\n").removeAll("\r")).matches
    }

    /** @return a regular expression String matching 's' inside another string, possibly multi-string */
    def regexPart = s"\\s*.*\\s*$s\\s*.*\\s*"
  }

  implicit class RegexMatch(r: Regex) {
    def matches(s: String): Boolean = r.pattern.matcher(s).matches
  }

}

object Regexes extends Regexes