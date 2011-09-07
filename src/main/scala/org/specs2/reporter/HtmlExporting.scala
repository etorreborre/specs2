package org.specs2
package reporter
import main.Arguments
import specification._

/**
 * Trait for exporting the specification as Html files
 */
trait HtmlExporting extends Exporting with HtmlPrinter {
  type ExportType = Unit
  
  def export(name: SpecName)(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    print(name, fragments)
  }

}