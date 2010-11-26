package org.specs2
package reporter

import io.FileWriter
import io.Paths._
import main.Arguments
import main.SystemProperties
import specification.ExecutedFragment

trait HtmlPrinter extends TextPrinter with FileWriter {
  private[specs2] lazy val outputDir: String = 
    SystemProperties.getOrElse("outDir", "target/specs2-reports/").dirPath
  
  override def print(klass: Class[_], fs: Seq[ExecutedFragment])(implicit args: Arguments) = {
    write(reportPath(klass)) { out => 
      printLines(fs, new HtmlResultOutput(out))
    }
  }
  
  def printLines(fs: Seq[ExecutedFragment], out: ResultOutput) = 
    super.printLines(fs).print(out) 
  
  def reportPath(klass: Class[_])(implicit args: Arguments) = {
    outputDir + klass.getName + ".html"
  }
}