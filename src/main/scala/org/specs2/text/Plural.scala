package org.specs2
package text

private[specs2] 
trait Plural {
  implicit def pluralize(s: String) = StringToPlural(s)
  case class StringToPlural(s: String) {
    def plural(v: Int) = if (v > 1) s+"s" else s    
    def plural(v: Long) = if (v > 1) s+"s" else s
    def bePlural(v: Int) = if (v > 1) s+"are" else s+"is"
    def bePlural(v: Long) = if (v > 1) s+"are" else s+"is"
  }
  implicit def intToQuantity(i: Int) = Quantity(i)
  case class Quantity(i: Int) {
	def qty(s: String) = i.toString + " " + s.plural(i)
	def qty_>(s: String)(a: Int = 0): Option[String] = if (i > a) Some(qty(s)) else None
  }
}
private[specs2] 
object Plural extends Plural
