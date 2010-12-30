package org.specs2
package text

/**
 * Abstraction of some text which may, or may not support a Markup syntax.
 *
 * It is primarily used for the html rendering of code in Example descriptions
 */
private[specs2]
sealed trait MarkupString {
  def asString = MarkupString.asString(this)
  def toHtml: String
  def append(s: String): MarkupString
}
case class CodeMarkup(text: String) extends MarkupString {
  def toHtml = (<code class="prettyprint">{text}</code>).toString
  def append(s: String) = CodeMarkup(text+s)
  override def toString = text
}
case class NoMarkup(text: String) extends MarkupString {
  def toHtml: String = text
  def append(s: String) = NoMarkup(text+s)
  override def toString = text
}
private[specs2]
object MarkupString {
  implicit def asString(m: MarkupString) = m match {
    case CodeMarkup(t) => t
    case NoMarkup(t) => t
  }
} 