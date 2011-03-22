package org.specs2
package text

/**
 * This trait provides functions to transform nouns to
 * their plural form
 *
 */
private[specs2] 
trait Plural {
  
  /** @return a Noun object which can be pluralized */
  implicit def noun(s: String) = Noun(s)
  case class Noun(s: String) {
    def plural(v: Int) = if (v > 1) s+"s" else s    
    def plural(v: Long) = if (v > 1) s+"s" else s
    def bePlural(v: Int) = if (v > 1) s+"are" else s+"is"
    def bePlural(v: Long) = if (v > 1) s+"are" else s+"is"
  }
  /** @return a Quantity which can be applied to a string to pluralize it */
  implicit def quantity(i: Int) = Quantity(i)
  case class Quantity(i: Int) {
    /** @return a pluralized string describing this quantity */
	  def qty(s: String) = i.toString + " " + s.plural(i)
    /** 
     * @return a Option with a pluralized string describing this quantity if it is 
     * greater than 0 
     */
	  def optQty(s: String): Option[String] = if (i > 0) Some(qty(s)) else None
    /**
     * @return a Option with a non-pluralized string describing this quantity if it is
     * greater than 0
     */
	  def optInvariantQty(s: String): Option[String] = if (i > 0) Some(i.toString+" "+s) else None
  }
  /** @return an Ordinal which can have a rank in a sequence */
  implicit def ordinal(i: Int) = Ordinal(i)
  case class Ordinal(i: Int) {
    /**
     * @return the proper postfix for an ordinal number
     */
    def th = i.toString +
      (if      (i == 1) "st"
       else if (i == 2) "nd"
       else if (i == 3) "rd"
       else             "th")
  }
}
private[specs2] 
object Plural extends Plural
