package org.specs2
package html

import java.io._

import io._
import Htmlx._
import specification.core._

import scala.io._
import scala.xml._
import scalaz._, Scalaz._
import Tree._
import data.Trees._
import xml.Nodex._

case class SpecHtmlPage(path: FilePath, content: String) {
  def addToc(toc: NodeSeq): SpecHtmlPage =
    copy(content =
      if (content.contains("<toc/>")) content.replace("<toc/>", toc.toString)
      else if (content.contains("<toc></toc>")) content.replace("<toc/>", toc.toString)
      else content)

  def createSubtoc: NodeSeq = {
    val items =
      body.headersTree.
        bottomUp { (h: Header, s: Stream[NodeSeq]) =>
        if (h.isRoot)
        // 'id' is the name of the attribute expected by jstree to "open" the tree on a specific node
          s.reduceNodes.updateHeadAttribute("id", path.name.name)
        else
          <li id={h.specId.toString}><a href={path.path}>{h.name}</a>
            { <ul>{s.toSeq}</ul> unless s.toSeq.isEmpty }
          </li>
      }.rootLabel

    <ul>{items}</ul>
  }

  def body = {
    XML.withSAXParser((new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl).newSAXParser)
      .load(new scala.xml.InputSource(new StringReader(content)))
  }
}

object SpecHtmlPage {
  
  def outputPath(outDir: DirectoryPath, spec: SpecStructure): FilePath =
    outDir | FileName.unsafe(spec.specClassName+".html")

}
