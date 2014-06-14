package org.specs2
package guide
package matchers

import form.Card

object XmlMatchers extends Card {
  def title = "Xml"
  def text = """
It is very useful to have literal Xml in Scala, it is even more useful to have matchers for it! If you want to use those matchers you need to extend the `org.specs2.matcher.XmlMatchers` trait:

 * `beEqualToIgnoringSpace` compares 2 Nodes, without considering spaces
 `<a><b/></a> must ==/(<a> <b/></a>)`
 `<a><b/></a> must beEqualToIgnoringSpace(<a> <b/></a>)`

 * `beEqualToIgnoringSpace` can also do an ordered comparison
 `<a><c/> <b/></a> must ==/(<a> <c/><b/></a>).ordered`

 * on the other hand `beEqualToIgnoringSpace` will not check attributes order
 `<n a="1" b="2"/> must ==/(<n b="2" a="1"/>)`

 * the `\\` matcher is an XPath-like matcher matching if a node is a direct child of another
 `<a><b/></a> must \\\\("b")`

 * You can also check attribute names
   `<a><b name="value"></b></a> must \\("b", "name")`

 * And attribute names and values as well (values are checked using a regular expression, use the <a href="http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html#quote(java.lang.String)">quote method</a> if you want an exact match)
 `<a><b n="v" n2="v2" n3="v3"></b></a> must \\("b", "n"->"v", "n2"->"v\\d")`

 * Or the content of a `Text` node
 `<a>hello</a> must \\("a") \\> "hello"` (alias `textIs`)
 `<a>hello</a> must \\("a") \\>~ "h.*"`  (alias `textMatches`)

 * The equivalent of `\\` for a "deep" match is simply `\\\\`
 `<a><s><c></c></s></a> must \\\\("c")`

"""
}
