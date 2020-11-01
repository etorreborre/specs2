package org.specs2
package text


/**
 * This trait provides functions to transform nouns to their plural form
 */
private[specs2]
trait Plural:

  /** @return a Noun object which can be pluralized */
  extension (s: String):
    def plural(vs: Iterable[Any]): String = s.plural(vs.size)
    def plural(v: Int): String     = if v > 1 then s+"s" else s
    def plural(v: Long): String    = if v > 1 then s+"s" else s

    def bePlural(v: Int): String = s.plural(v) + " " + beVerbPlural(v)
    def bePlural(v: Long): String = s.plural(v) + " " + beVerbPlural(v)

  def beVerbPlural(v: Int): String  = if v > 1 then "are" else "is"
  def beVerbPlural(v: Long): String = if v > 1 then "are" else "is"

  /** @return a Quantity which can be applied to a string to pluralize it */
  extension (i: Int):
    /** @return a pluralized string describing this quantity */
    def qty(s: String): String = i.toString + " " + s.plural(i)
    /** @return a pluralized string describing this quantity with the be verb */
    def beQty(s: String): String = i.toString + " " + s.bePlural(i)
    /** @return a Option with a pluralized string describing this quantity if it is greater than 0 */
    def optQty(s: String): Option[String] = if i > 0 then Some(qty(s)) else None
    /** @return a pluralized string describing this quantity if it is greater than 0 or an empty string */
    def strictlyPositiveOrEmpty(s: String): String = if i > 0 then qty(s) else ""
    /**
     * @return a Option with a non-pluralized string describing this quantity if it is
     * greater than 0
     */
    def optInvariantQty(s: String): Option[String] = if i > 0 then Some(i.toString+" "+s) else None

    /**
     * @return the proper postfix for an ordinal number
     */
    def th: String = i.toString +
      (if      i == 1 then "st"
       else if i == 2 then "nd"
       else if i == 3 then "rd"
       else             "th")

private[specs2]
object Plural extends Plural
