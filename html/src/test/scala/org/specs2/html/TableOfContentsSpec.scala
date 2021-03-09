package org.specs2
package html

import TableOfContents.*
import io.*
import specification.core.{Env, OwnExecutionEnv, SpecStructure}

import scala.xml.NodeSeq
import matcher.XmlMatchers

class TableOfContentsSpec(val env: Env) extends Specification with HtmlDocuments with XmlMatchers with OwnExecutionEnv { def is = s2"""

 The table of contents is created from the specifications and the generated html files

 Creating a table of content for a html document
    creates an unordered list from the html headers
      as nested <li/> lists corresponding to the hierarchy of the document headers      $toc1
      each <li/> element has
        the header text as text                                                         $toc2
        an url+anchor referencing the header name                                       $toc3
        an id attribute with the spec id. the id attribute is expected by jstree        $toc4

"""

  def toc1 = addToc(aBodyWithHeaders) must \\("li") \\ ("ul") \ ("li")
  //    <li><a href="http://specs2.org/#title_123456">title</a>
  //      <ul><li><a href="http://specs2.org/#a+header_123456">a header</a></li>
  //      </ul>
  //    </li>
  def toc2 = addToc(aBodyWithHeaders) must \\ ("li") \ ("a") \> "Table of conten..."
  def toc3 = addToc(aBodyWithHeaders) must \\ ("li") \ ("a", "href" -> "UserGuide.html")
  def toc4 = addToc(aBodyWithHeaders) must \\ ("li", "id")

  def addToc(body: NodeSeq) =
    val page = SpecHtmlPage(SpecStructure.empty(getClass), outDir | "UserGuide.html", outDir, body.toString)
    createToc(List(page), outDir, entryMaxSize = 18).apply(page)

  val outDir = DirectoryPath.unsafe("guide")
}
