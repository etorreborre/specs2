package org.specs2
package reporter

import java.io.Writer
import scala.xml.{Xhtml, NodeSeq}
import main.Arguments
import HtmlUrls._
import internal.scalaz.Scalaz
import Scalaz._

trait HtmlFileWriter extends OutputDir {

  def writeFiles(implicit args: Arguments = Arguments()) = (htmlFiles: Seq[HtmlFile]) => {
    copyResources()
    val files = htmlFiles.filter(_.nonEmpty)
    files.foreach(writeFile)
    files.foreach(checkUrls(args, files))
  }

  protected def writeFile = (file: HtmlFile) => {
    fileWriter.write(reportPath(file.url))(writeXml(file.xml))
  }

  protected def checkUrls(args: Arguments, others: Seq[HtmlFile]) = (file: HtmlFile) => {
    if (args.report.checkUrls) {
      val result = HtmlUrls.check(file.xml, Map(others.map(f => (f.url, f.xml)):_*), outputDir)
      if (!result.isSuccess) println(result)
    }
    file
  }

  /** write the xml output to a Writer */
  protected def writeXml(xml: NodeSeq)(out: Writer) { out.write(Xhtml.toXhtml(xml)) }

   /** copy css and images file to the output directory */
  protected def copyResources() {
    Seq("css", "images", "css/themes/default").foreach(fileSystem.copySpecResourcesDir(_, outputDir))
  }
}