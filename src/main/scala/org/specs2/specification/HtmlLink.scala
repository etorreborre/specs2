package org.specs2
package specification

import io.Paths._
/**
 * The HtmlLink class represents a link in a specification with an identifier (either a spec name, or a string) and
 * link elements.
 */
abstract class HtmlLink(val url: String, val beforeText: String, val linkText: String, val afterText: String, val tip: String) {
  def is(name: SpecName) = false

  def urlIs(url: String)    : HtmlLink
  def baseDirIs(dir: String): HtmlLink
}
case class SpecHtmlLink(name: SpecName,
                        override val beforeText: String = "",
                        override val linkText: String = "",
                        override val afterText: String = "",
                        override val tip: String = "") extends
    HtmlLink(name.url, beforeText, linkText, afterText, tip) {
  override def is(n: SpecName) = name.id == n.id

  def urlIs(url: String)     = copy(name = name.urlIs(url))
  def baseDirIs(dir: String) = copy(name = name.baseDirIs(dir))
}

case class UrlHtmlLink(override val url: String,
                       override val beforeText: String = "",
                       override val linkText: String   = "",
                       override val afterText: String  = "",
                       override val tip: String        = "") extends
   HtmlLink(url, beforeText, linkText, afterText, tip) {

  def urlIs(u: String)       = copy(url = u)
  def baseDirIs(dir: String) = copy(url = url.rebase(dir))
}


object HtmlLink {
 def apply(name: SpecName, beforeText: String = "", linkText: String = "", afterText: String = "", tip: String = ""): SpecHtmlLink  =
   new SpecHtmlLink(name, beforeText, linkText, afterText, tip)

  def apply(s: SpecificationStructure): SpecHtmlLink = HtmlLink(s.content)
  def apply(f: Fragments): SpecHtmlLink              = HtmlLink(f.specName)
  def apply(specName: SpecName)                      = SpecHtmlLink(specName, linkText = specName.name)
}
