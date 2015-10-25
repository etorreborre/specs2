package org.specs2
package html

import Htmlx._
import specification.Grouped
import matcher._

class HtmlxSpec extends Spec with HtmlDocuments with Grouped with TypedEqual { def is = s2"""

  headers functions
  ${ isHeader(<h1/>) must beTrue }
  ${ isHeader(<h2/>) must beTrue }
  ${`headersToTree builds a Tree of headers from a html document`}
  ${`headersToTree builds a Tree of headers - 2`}
  ${ (<h2 id="1"/> ++ <h3/>).updateHeadAttribute("id", 3) === (<h2 id='3'/> ++ <h3/>) }
  ${ <h2>hello</h2>.addHeadersAnchors.toString must beMatching("<a name=\"hello\"><h2>hello</h2></a>") }

  the headers methods
    collects all headers of a document  ${g1.e1}

  nodeText extracts the text from a Node
  ${ nodeText(<h2>Hello</h2>) must_== "Hello"}

  urls extracts all urls from <a/> nodes
  ${ urls(<a href="www.google.com">hi</a>) must_== Seq("www.google.com") }

  Anchor names which are build for headers must be unique and the same in the header tree ${g2.e1}
                                                                                                               """

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

  "headers" - new g1 {
    e1 := headers(<body><h1>title1</h1>Some text <h2>title2</h2>Some other text</body>) must_== (<h1>title1</h1> ++ <h2>title2</h2>)
  }

  "anchors" - new g2 {
    e1 := {
      val body = <body><h1>Welcome</h1><h2>hello</h2></body>
      val anchorRegex = "hello_(\\d*)".r
      // anchor id for the "hello" header
      val id1 = anchorRegex.findFirstIn(body.addHeadersAnchors.toString)
      // anchor id for the "hello" header in the tree
      val id2 = anchorRegex.findFirstIn(body.headersTree.flatten.toSeq(1).anchorName(".").toString)
      id1 === id2
    }
  }

}
