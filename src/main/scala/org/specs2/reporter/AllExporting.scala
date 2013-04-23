package org.specs2
package reporter

import specification.{ExecutingSpecification, ExecutedSpecification, SpecificationStructure}
import main.Arguments
import scalaz.Scalaz._

/**
 * This trait can be mixed in a reporter to allow the exporting of an executed specification to many different formats:
 * html, junit xml, markup, custom,...
 */
trait AllExporting extends Reporter with Exporters {

  override def report(spec: SpecificationStructure)(implicit arguments: Arguments): ExecutedSpecification =
    spec |> select |> sequence |> execute |> export

  // if the results need to be exported to the console, we first do that making sure that the storing of statistics occurs in
  // parallel to the export. This way, the results are displayed as soon as executed
  // then we take the result of storing the stats, which sets up more information on the SpecStart/SpecEnd, and pass it
  // to other exporters like the html exporter for example. This exporter needs this additional information to properly display
  // index pages and total statistics
  override def export(implicit arguments: Arguments): ExecutingSpecification => ExecutedSpecification = (executing: ExecutingSpecification) => {
    exportConsole(arguments.contains) match {
      case Some(e) => {
        val storeAndExport = (spec: ExecutingSpecification) => Seq(store, e.export).par.map(_(spec)).head.asInstanceOf[ExecutingSpecification]
        val executed = executing |> storeAndExport
        val args = arguments <| executed.arguments
        exportAll(exporters(arguments).filterNot(_ == TextExporting))(args).apply(executed)
      }
      case None => {
        val executed = executing |> store
        val args = arguments <| executed.arguments
        exportAll(exporters(arguments))(args).apply(executed)
      }
    }

  }
}
