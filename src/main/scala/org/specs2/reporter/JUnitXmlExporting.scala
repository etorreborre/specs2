package org.specs2
package reporter

import specification._
import io.{FileWriter, FileSystem}
import io.Paths._
import main.{SystemProperties, Arguments}
import org.junit.runner.Description

/**
* Trait for exporting the specification as JUnit xml files
*/
trait JUnitXmlExporting extends Exporting with JUnitXmlPrinter {
  type ExportType = Unit

  def export(implicit args: Arguments): ExecutingSpecification => ExportType = (spec: ExecutingSpecification) => {
    print(spec.execute.name, spec.execute.fragments)
  }

  /** the file system is used to open the file to write */
  private[specs2] lazy val fileSystem = new FileSystem {}
  /** the file writer is used to open the file to write */
  private[specs2] lazy val fileWriter = new FileWriter {}

  /**
   * the output directory is either defined by a specs2 system variable
   * or chosen as a reports directory in the standard maven "target" directory
   */
  private[specs2] lazy val outputDir: String = SystemProperties.getOrElse("junit.outDir", "target/test-reports/").dirPath

  /**
   * print a sequence of executed fragments for a given specification class into a html
   * file
   * the name of the html file is the full class name
   */
  def print(name: SpecName, fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    lazy val suite = testSuite(name, fs)
    fileWriter.write(filePath(suite.description)) { out => suite.flush(out) }
  }

  def filePath(desc: Description) = outputDir + desc.getClassName + ".xml"

}

object JUnitXmlExporting extends JUnitXmlExporting