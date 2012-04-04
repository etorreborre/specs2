package org.specs2
package html
import Htmlx._

class HtmlxSpec extends Specification with HtmlDocuments { def is =

  "headers functions"                                                                                                   ^
    { isHeader(<h1/>) must beTrue }                                                                                     ^
    { isHeader(<h2/>) must beTrue }                                                                                     ^
    `headersToTree builds a Tree of headers from a html document`                                                       ^
    `headersToTree builds a Tree of headers - 2`                                                                        ^
    { (<h2 id="1"/> ++ <h3/>).updateHeadAttribute("id", 3) === (<h2 id="3"/> ++ <h3/>) }                                ^
    { <h2>hello</h2>.addHeadersAnchors === <a name="hello"><h2>hello</h2></a> }                                         ^
                                                                                                                        p^
  "nodeText extracts the text from a Node"                                                                              ^
    { nodeText(<h2>Hello</h2>) must_== "Hello"}                                                                         ^
    { nodeText(<h2>Hello<notoc>world</notoc></h2>) must_== "Hello" }                                                    ^
                                                                                                                        p^
  "urls extracts all urls from <a/> nodes"                                                                              ^
    { urls(<a href="www.google.com">hi</a>) must_== Seq("www.google.com") }                                             ^
                                                                                                                        end

  def `headersToTree builds a Tree of headers from a html document` =
    aBodyWithTwoH3HeadersAndOneH4Each.headersTree.drawTree.trim must_==
    """.title
       .|
       .+- a h3 header
       .|  |
       .|  `- first h4
       .|
       .`- another h3 header
       .   |
       .   `- second h4""".stripMargin('.').replace("\r", "")

  def `headersToTree builds a Tree of headers - 2` =
    aBodyWithAH3ThenAH2Header.headersTree.drawTree.trim must_==
    """.|
       .+- a h3 header
       .|
       .`- a h2 header""".stripMargin('.').replace("\r", "")

}