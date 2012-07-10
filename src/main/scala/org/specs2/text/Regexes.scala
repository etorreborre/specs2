package org.specs2
package text

import control.Exceptions._
import java.util.regex.Pattern
import Trim._

/**
 * This trait provides utility functions for working with regexes
 */
private[specs2]
trait Regexes {

  implicit def regexed(s: String): Regexed = Regexed(s)
  case class Regexed(s: String) {

    /**
     * matchesSafely a pattern p. If p cannot be compiled, then it is quoted
     * if the string s is enclosed with characters, they can be excluded before the quotation is done
     */
    def matchesSafely(p: String, enclosing: String = "") = {
      val pattern = tryOrElse(Pattern.compile(p))(Pattern.compile(enclosing+Pattern.quote(p.trimEnclosing(enclosing))+enclosing))
      pattern.matcher(s.removeAll("\n").removeAll("\r")).matches
    }
  }
}

private[specs2]
object Regexes extends Regexes