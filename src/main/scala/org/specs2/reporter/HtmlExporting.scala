package org.specs2
package reporter
import main.Arguments
import specification._

trait HtmlExporting extends Exporting with HtmlPrinter {
  type ExportType = Unit
  
  def export(klass: Class[_])(implicit args: Arguments) = (fragments: Seq[ExecutedFragment]) => {
    print(klass, fragments)
  }

}