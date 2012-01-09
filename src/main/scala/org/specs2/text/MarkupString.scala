package org.specs2
package text

/**
 * Abstraction of some text which may, or may not support a Markup syntax.
 *
 * It is primarily used for the html rendering of code in Example descriptions
 */
private[specs2]
trait MarkupString {
  def toXml: scala.xml.Elem
  def toHtml: String = toXml.toString
}
case class CodeMarkup(text: String) extends MarkupString {
  def asString = text
  def toXml = <code class="prettyprint">{text}</code>
  override def toString = text
}
case class NoMarkup(text: String) extends MarkupString {
  def toXml = <t>{text}</t>
  override def toHtml: String = text
  override def toString = text
}
case class EmptyMarkup() extends MarkupString {
  def toXml = <t></t>
  override def toHtml: String = ""
  override def toString = ""
}

