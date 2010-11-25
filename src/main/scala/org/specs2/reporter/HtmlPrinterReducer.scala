package org.specs2
package reporter
import io.FileWriter
import specification.ExecutedFragment

trait HtmlPrinterReducer extends TextPrinterReducer with FileWriter {
  
  override def print(fs: Seq[ExecutedFragment]) = {
    write("specs-report") { out => 
      super.print(fs)(new HtmlResultOutput(out)) 
    }
  }
}