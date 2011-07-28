package org.specs2
package specification

import execute._

/**
 * The HtmlLink class represents a link in a specification with an identifier (either a spec name, or a string) and
 * link elements.
 */
abstract class HtmlLink(val url: String, val beforeText: String, val linkText: String, val afterText: String, val tip: String) {
  def is(name: SpecName) = false
}
case class SpecHtmlLink(val name: SpecName,
                        override val beforeText: String = "",
                        override val linkText: String = "",
                        override val afterText: String = "",
                        override val tip: String = "") extends
   HtmlLink(name.url, beforeText, linkText, afterText, tip) {
  override def is(n: SpecName) = name.id == n.id
}

case class UrlHtmlLink(override val url: String,
                       override val beforeText: String,
                       override val linkText: String,
                       override val afterText: String,
                       override val tip: String) extends
   HtmlLink(url, beforeText, linkText, afterText, tip)


object HtmlLink {
 def apply(name: SpecName, beforeText: String = "", linkText: String = "", afterText: String = "", tip: String = ""): HtmlLink  =
   new SpecHtmlLink(name, beforeText, linkText, afterText, tip)

  def apply(s: SpecificationStructure): HtmlLink = SpecHtmlLink(s.content.specName, linkText = s.content.name)
}
