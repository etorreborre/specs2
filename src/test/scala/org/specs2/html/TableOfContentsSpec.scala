package org.specs2
package html
import TableOfContents._
import matcher.DataTables

class TableOfContentsSpec extends Specification with DataTables with HtmlDocuments { def is =

  "Creating a table of content for a body creates nothing when there are"                                               ^
    `no toc elements`                                                                                                  ^
    `no headers except the title`                                                                                       ^
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
                                                                                                                        end

  def `no toc elements` = addToc(aBodyWithHeadersButNoToc) must not \\(<li/>)
  def `no headers except the title` = addToc(aBodyWithNoHeaders) must not \\(<li/>)

  def `creates one anchor` = addToc(aBodyWithOneH3Header) must \\("a", "name"->"a+h3+header")
  def `create 2 anchors` = addToc(aBodyWithTwoH3Headers) must \\("a", "name"->"another+h3+header")
  def `creates a link to the anchor in a <li/> element` = addToc(aBodyWithOneH3Header) must \\("li")
  def `create a link to the anchors in 2 <li/> elements` =
    addToc(aBodyWithTwoH3Headers) must \\("li") \\ ("a", "href"->"#another+h3+header")

  def `create links to the anchors with a nested <ul> element` =
    addToc(<body><toc/>{aBodyWithTwoH3HeadersAndOneH4Each}</body>) must \\ {
      <ul>
        <li id="a h3 header"><a href="#a+h3+header">a h3 header</a>
          <ul><li id="first h4"><a href="#first+h4">first h4</a></li></ul>
        </li>
        <li id="another h3 header"><a href="#another+h3+header">another h3 header</a>
          <ul><li id="second h4"><a href="#second+h4">second h4</a></li></ul>
        </li>
      </ul>
    }

}
