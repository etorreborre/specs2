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

}
private[specs2] object Quote extends Quote 