package org.specs2
package reporter

import main.Arguments
import specification._
import internal.scalaz.Scalaz._

/**
 * Trait for exporting the specification as Html files
 */
trait HtmlExporting extends Exporting with HtmlPrinter with HtmlFileWriter {

  def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    print(executed) |> writeFiles
    executed
  }
}
object HtmlExporting extends HtmlExporting