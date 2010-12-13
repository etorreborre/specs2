package org.specs2
package specification

import execute._


case class HtmlLink(url: String, beforeText: String, linkText: String, afterText: String, tip: String, result: Result)

object HtmlLink {
 def apply(s: SpecificationStructure, beforeText: String = "", linkText: String = "", afterText: String = "", tip: String = "", result: Result = Success()): HtmlLink  =
   HtmlLink.fromClass(s.getClass, beforeText, linkText, afterText, tip, result)

 def fromClass(klass: Class[_], beforeText: String = "", linkText: String = "", afterText: String = "", tip: String = "", result: Result = Success()): HtmlLink  =
   new HtmlLink(klass.getName + ".html", beforeText, linkText, afterText, if (tip.isEmpty) klass.getSimpleName else tip, result)
}
