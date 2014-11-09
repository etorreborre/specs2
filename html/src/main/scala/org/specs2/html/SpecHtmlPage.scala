package org.specs2
package html

import io._
import specification.core._

case class SpecHtmlPage() {

}

object SpecHtmlPage {
  
  def outputPath(outDir: DirectoryPath, spec: SpecStructure): FilePath =
    outDir | FileName.unsafe(spec.specClassName+".html")

}
