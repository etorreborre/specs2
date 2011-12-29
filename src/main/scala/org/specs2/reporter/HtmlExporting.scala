package org.specs2
package reporter
import main.Arguments
import specification._
import internal.scalaz.Scalaz._

/**
 * Trait for exporting the specification as Html files
 */
trait HtmlExporting extends Exporting with HtmlPrinter with HtmlFileWriter {
  type ExportType = Unit
  
  def export(implicit args: Arguments): ExecutingSpecification => ExportType = (spec: ExecutingSpecification) =>
    print(spec.execute) |> writeFiles

}
object HtmlExporting extends HtmlExporting