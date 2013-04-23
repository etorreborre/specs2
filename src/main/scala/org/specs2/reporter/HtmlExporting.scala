package org.specs2
package reporter

import main.Arguments
import specification._
import scalaz.Scalaz._

/**
 * Trait for exporting the specification as Html files
 */
trait HtmlExporting extends Exporting with HtmlPrinter with HtmlFileWriter {

  def export(implicit arguments: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    val args = arguments <| executed.arguments
    print(executed)(args) |> writeFiles(args)
    executed
  }
}
object HtmlExporting extends HtmlExporting