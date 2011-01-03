package org.specs2.text

/**
 * Quote and unquote strings
 *
 */
private[specs2] 
trait Quote {
  def q(a: Any) = "'"+a+"'" 

  /** @return an object.toString() without quotes (used in messages creation) */
  def unq(a: Any)  = if (null == a) "null" else a.toString

  implicit def prefixed(s: String) = new Prefixed(s)
  class Prefixed(s: String) {
    def prefix(separator: String, other: String) = Seq(s, other).filter(_.nonEmpty).mkString(separator)
  }
}
private[specs2] object Quote extends Quote 