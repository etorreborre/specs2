package org.specs2
package html

import io._
import specification.core._

import scala.xml.NodeSeq

case class SpecHtmlPage(path: FilePath, content: String) {
  def addToc(toc: NodeSeq): SpecHtmlPage =
    this
}

object SpecHtmlPage {
  
  def outputPath(outDir: DirectoryPath, spec: SpecStructure): FilePath =
    outDir | FileName.unsafe(spec.specClassName+".html")

}
