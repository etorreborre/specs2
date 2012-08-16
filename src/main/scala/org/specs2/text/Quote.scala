package org.specs2.text

import NotNullStrings._

/**
 * Quote and unquote strings
 *
 */
private[specs2] 
trait Quote {
  def q(a: Any) = "'"+a.notNull+"'"

  /** @return an object.toString() without quotes (used in messages creation) */
  def unq(a: Any)  = a.notNull

  implicit def prefixed(s: String) = new Prefixed(s)
  class Prefixed(s: String) {
    def prefix(separator: String, other: String) = Seq(s, other).filter(_.nonEmpty).mkString(separator)
  }
}
private[specs2] object Quote extends Quote 