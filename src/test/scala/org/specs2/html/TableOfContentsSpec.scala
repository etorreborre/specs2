package org.specs2
package html
import TableOfContents._
import scala.xml.NodeSeq

class TableOfContentsSpec extends Specification with HtmlDocuments { def is =

  "Creating a table of content for a html document"                                               ^
    "creates an unordered list from the html headers"                                             ^
      "as nested <li/> lists corresponding to the hierarchy of the document headers"              ! e1^
      "each <li/> element has"                                                                    ^
        "the header text as text"                                                                 ! e2^
        "an url+anchor referencing the header name"                                               ! e3^
        "an id attribute with the spec id. the id attribute is expected by jstree"                ! e4^
    "injects sub-table of contents where there are <subtoc/> tags in the original document"       ! e5^
                                                                                                  end

  def e1 = addToc(aBodyWithHeaders) must \\("li") \\ ("ul") \ ("li")
//    <li><a href="http://specs2.org/#title">title</a>
//      <ul><li><a href="http://specs2.org/#a+header">a header</a></li>
//      </ul>
//    </li>
  def e2 = addToc(aBodyWithHeaders) must \\ ("li") \ ("a") \> "title"
  def e3 = addToc(aBodyWithHeaders) must \\ ("li") \ ("a", "href" -> "../guide/#title")
  def e4 = addToc(aBodyWithHeaders) must \\ ("li", "id")
  def e5 = {
    val subtoc = <a href="http://specs2.org/#other" />
    addToc(aBodyWithHeadersAndASubtoc, Map(SpecId("123") -> subtoc)) must \\ ("li") \\ ("a", "href" -> "http://specs2.org/#other")
  }

  def addToc(body: NodeSeq, subtocs: Map[SpecId, NodeSeq] = Map()) =
    tocItemList(body, ".", "guide/", SpecId("specName"), subtocs)

}
