package org.specs2
package html
import TableOfContents._
import matcher.DataTables

class TableOfContentsSpec extends Specification with DataTables { def is =

  "Creating a table of content for a body with"                                                                         ^
    `no toc elements creates nothing`                                                                                   ^
    `no headers except the title creates nothing`                                                                       ^
                                                                                                                        p^
    "one h2 header"                                                                                                     ^
      `creates one anchor`                                                                                              ^
      `creates a link to the anchor in a <li/> element`                                                                 ^
                                                                                                                        p^
    "2 h4 headers"                                                                                                      ^
      `create 2 anchors`                                                                                                ^
      `create a link to the anchors in 2 <li/> elements`                                                                ^
                                                                                                                        p^
    "2 h3 headers with one h4 header each"                                                                              ^
      `create links to the anchors with a nested <ul> element`                                                          ^
                                                                                                                        endp^
  "support functions"                                                                                                   ^
    { isHeader(<h1/>) must beTrue }                                                                                     ^
    { isHeader(<h2/>) must beTrue }                                                                                     ^
    `headersToTree builds a Tree of headers`                                                                            ^
    `headersToTree builds a Tree of headers - 2`                                                                        ^
                                                                                                                        end

  val aBodyWithHeadersButNoToc           = <body><h1>title</h1>text with <h2>a header</h2></body>
  val aBodyWithNoHeaders                 = <body><h1>title</h1>text with <toc/><i>other nodes</i></body>
  val aBodyWithH1HeadersOnly             = <body><h1>title</h1>text with <toc/><h1>a h1 header</h1></body>
  val aBodyWithH2HeadersOnly             = <body><h1>title</h1>text with <toc/><h2>a h2 header</h2></body>
  val aBodyWithOneH3Header               = <body><h1>title</h1>text with <toc/><h3>a h3 header</h3></body>
  val aBodyWithTwoH3Headers              = <body><h1>title</h1>text with <toc/><h3>a h3 header</h3>other text<h3>another h3 header</h3></body>
  val aBodyWithTwoH3HeadersAndOneH4Each  =
    <h1>title</h1>             ++
      <h3>a h3 header</h3>     ++
      <h4>first h4</h4>        ++
    <h3>another h3 header</h3> ++
      <h4>second h4</h4>

  val aBodyWithAH3ThenAH2Header  =
      <h3>a h3 header</h3>     ++
      <h2>a h2 header</h2>

  def `no toc elements creates nothing` = addToc(aBodyWithHeadersButNoToc) must not \\(<li/>)
  def `no headers except the title creates nothing` = addToc(aBodyWithNoHeaders) must not \\(<li/>)

  def `creates one anchor` = addToc(aBodyWithOneH3Header) must \\("a", "name"->"a+h3+header")
  def `create 2 anchors` = addToc(aBodyWithTwoH3Headers) must \\("a", "name"->"another+h3+header")
  def `creates a link to the anchor in a <li/> element` = addToc(aBodyWithOneH3Header) must \\("li")
  def `create a link to the anchors in 2 <li/> elements` =
    addToc(aBodyWithTwoH3Headers) must \\("li") \\ ("a", "href"->"#another+h3+header")

  def `create links to the anchors with a nested <ul> element` =
    addToc(<body><toc/>{aBodyWithTwoH3HeadersAndOneH4Each}</body>) must \\ {
      <ul>
        <li><a href="#a+h3+header">a h3 header</a>
          <ul><li><a href="#first+h4">first h4</a></li></ul>
        </li>
        <li><a href="#another+h3+header">another h3 header</a>
          <ul><li><a href="#second+h4">second h4</a></li></ul>
        </li>
      </ul>
    }

  def `headersToTree builds a Tree of headers` = headersToTree(aBodyWithTwoH3HeadersAndOneH4Each).toTree.drawTree.trim must_==
    """.title
       .|
       .+- a h3 header
       .|  |
       .|  `- first h4
       .|
       .`- another h3 header
       .   |
       .   `- second h4""".stripMargin('.').replace("\r", "")

  def `headersToTree builds a Tree of headers - 2` = headersToTree(aBodyWithAH3ThenAH2Header).toTree.drawTree.trim must_==
    """.|
       .+- a h3 header
       .|
       .`- a h2 header""".stripMargin('.').replace("\r", "")

}
