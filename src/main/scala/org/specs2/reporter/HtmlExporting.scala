package org.specs2
package reporter
import main.Arguments
import specification._

trait HtmlExporting extends Exporting with HtmlPrinter {
  type ExportType = Unit
  
  def export(s: SpecificationStructure)(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    print(s, fragments)
  }

}