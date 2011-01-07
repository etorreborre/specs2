package org.specs2
package html
import TableOfContents._
import scalaz.Scalaz._

class TableOfContentsSpec extends SpecificationWithJUnit { def is =

  "Creating a table of content for a body with"                                      ^
    `no toc elements creates nothing`                                                ^
    `no headers creates nothing`                                                     ^
    `one h1 header creates nothing`                                                  ^
    `one h2 header creates nothing`                                                  ^
                                                                                     p^
    "one h2 header"                                                                  ^
      `creates one anchor`                                                           ^
      `creates a link to the anchor in a <li/> element`                              ^
                                                                                     p^
    "2 h4 headers"                                                                   ^
      `create 2 anchors`                                                             ^
      `create a link to the anchors in 2 <li/> elements`                             ^
                                                                                     p^
    "2 h3 headers with one h4 header each"                                           ^
      `create links to the anchors with a nested <ul> element`                       ^
                                                                                     endp^
  "support functions"                                                                ^
    { isHeader(<h1/>, (_:Int) >1) must beFalse }                                     ^
    { isHeader(<h2/>, (_:Int) >1) must beTrue }                                      ^
    `headersToTree builds a Tree of headers`                                         ^
                                                                                     end

  val aBodyWithHeadersButNoToc           = <body>text with <h2>a header</h2></body>
  val aBodyWithNoHeaders                 = <body>text with <toc/><i>other nodes</i></body>
  val aBodyWithH1HeadersOnly             = <body>text with <toc/><h1>a h1 header</h1></body>
  val aBodyWithH2HeadersOnly             = <body>text with <toc/><h2>a h2 header</h2></body>
  val aBodyWithOneH3Header               = <body>text with <toc/><h3>a h3 header</h3></body>
  val aBodyWithTwoH3Headers              = <body>text with <toc/><h3>a h3 header</h3>other text<h3>another h3 header</h3></body>
  val aBodyWithTwoH3HeadersAndOneH4Each  = <body>text with <toc/>
    <h3>a h3 header</h3>
      text
      <h4>first h4</h4>
      text
    <h3>another h3 header</h3>
      text
      <h4>second h4</h4>
      text
  </body>

  def `no toc elements creates nothing` = addToc(aBodyWithHeadersButNoToc) must not \\(<li/>)
  def `no headers creates nothing` = addToc(aBodyWithNoHeaders) must not \\(<li/>)
  def `one h1 header creates nothing` = addToc(aBodyWithH1HeadersOnly) must not \\(<li/>)
  def `one h2 header creates nothing` = addToc(aBodyWithH2HeadersOnly) must not \\(<li/>)

  def `creates one anchor` = addToc(aBodyWithOneH3Header) must \\("a", "name"->"a+h3+header")
  def `create 2 anchors` = addToc(aBodyWithTwoH3Headers) must \\("a", "name"->"another+h3+header")
  def `creates a link to the anchor in a <li/> element` = addToc(aBodyWithOneH3Header) must \\("li")
  def `create a link to the anchors in 2 <li/> elements` =
    addToc(aBodyWithTwoH3Headers) must \\("li") \\ ("a", "href"->"#another+h3+header")

  def `create links to the anchors with a nested <ul> element` =
    addToc(aBodyWithTwoH3HeadersAndOneH4Each) must \\ {
      <ul>
        <li><a href="#a+h3+header">a h3 header</a></li>
          <ul><li><a href="#first+h4">first h4</a></li></ul>
        <li><a href="#another+h3+header">another h3 header</a></li>
         <ul><li><a href="#second+h4">second h4</a></li></ul>
      </ul>
    }

  def `headersToTree builds a Tree of headers` = headersToTree(aBodyWithTwoH3HeadersAndOneH4Each).toTree.drawTree.trim must_==
    """.|
       .+- a h3 header
       .|  |
       .|  `- first h4
       .|
       .`- another h3 header
       .   |
       .   `- second h4""".stripMargin('.').replace("\r", "")
}