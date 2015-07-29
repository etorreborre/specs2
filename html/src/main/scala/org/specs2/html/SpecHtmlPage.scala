package org.specs2
package html

import java.io._

import io._
import specification.core._
import scala.xml._

case class SpecHtmlPage(specification: SpecStructure, path: FilePath, outDir: DirectoryPath, content: String) {

  /** @return the class name of the specification */
  def className = specification.header.className

  /** @return the simple class name of the specification */
  def simpleName = specification.header.simpleName

  /** @return a name that is transform in the same way Pandoc creates identifiers for headers */
  def pandocName = className.toLowerCase.replace(".", "-")

  /** @return the title of the specification */
  def showWords = specification.header.showWords

  def addToc(toc: NodeSeq): SpecHtmlPage = {
    val replacedToc = content.replace("<toc/>", toc.toString)
    copy(content = replacedToc)
  }

  def relativePath: FilePath =
    path.relativeTo(outDir)

  def body: NodeSeq =
    parse(content)

  private def parse(string: String): NodeSeq =
    XML.withSAXParser((new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl).newSAXParser)
      .load(new scala.xml.InputSource(new StringReader(string)))
}

object SpecHtmlPage {
  
  def outputPath(outDir: DirectoryPath, spec: SpecStructure): FilePath =
    outputPath(outDir, spec.specClassName)

  def outputPath(outDir: DirectoryPath, specClassName: String): FilePath =
    outDir | FileName.unsafe(specClassName+".html")

}
