package org.specs2
package reporter

import java.io.Writer
import scala.xml.{Xhtml, NodeSeq}


trait HtmlFileWriter extends OutputDir {

  def writeFiles = (htmlFiles: Seq[HtmlFile]) => {
    copyResources()
    htmlFiles.filter(_.nonEmpty) foreach writeFile
  }

  protected def writeFile = (file: HtmlFile) => {
    fileWriter.write(reportPath(file.url))(writeXml(file.xml))
  }

 /** write the xml output to a Writer */
  protected def writeXml(xml: NodeSeq)(out: Writer) = out.write(Xhtml.toXhtml(xml))

   /** copy css and images file to the output directory */
  protected def copyResources() {
    Seq("css", "images", "css/themes/default").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
  }
}