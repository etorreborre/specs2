package org.specs2
package reporter

import reflect.Classes
import main.Arguments
import specification.{ExecutedSpecification, ExecutingSpecification}

private [specs2]
trait Exporters {
  type EE = ExecutingSpecification => ExecutedSpecification

  def exportToOthers(arguments: Arguments): EE = exportToOthers(arguments, (s: String) => arguments.commandLine.contains(s))

  def exportToOthers(args: Arguments, accept: String => Boolean): EE = (spec: ExecutingSpecification) => {
    exportToOthers(exporters(accept)(args))(args)(spec)
  }

  def exportToOthers(exporters: Seq[Exporting])(implicit arguments: Arguments): EE = (spec: ExecutingSpecification) => {
    val args = arguments.commandLineFilterNot("html", "junitxml", "console", "notifier", "exporter")
    exporters.foreach(_.export(args)(spec))
    spec.executed
  }

  def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] =
    Seq(exportHtml(accept),
        exportJUnitxml(accept),
        exportNotifier(accept),
        exportCustom(accept),
        exportConsole(accept)).flatten

  def notifierExporter(arguments: Arguments): Option[Exporting] =
    Classes.createObject[Notifier](arguments.report.notifier, true).map(n => new NotifierExporting { val notifier = n })

  def customExporter(arguments: Arguments): Option[Exporting] = Classes.createObject[Exporting](arguments.report.exporter, true)

  protected def exporter(condition: Boolean)(e: =>Exporting) = if (condition) Some(e) else None
  protected def optionalExporter(condition: Boolean)(e: Option[Exporting]) = if (condition) e else None

  def exportHtml(accept: String => Boolean)    (implicit arguments: Arguments) = exporter(accept("html"))(HtmlExporting)
  def exportJUnitxml(accept: String => Boolean)(implicit arguments: Arguments) = exporter(accept("junitxml"))(JUnitXmlExporting)
  def exportConsole(accept: String => Boolean) (implicit arguments: Arguments) = exporter(accept("console"))(TextExporting)

  def exportNotifier(accept: String => Boolean)(implicit arguments: Arguments) =
    optionalExporter(accept("notifier") || !arguments.report.notifier.isEmpty)(notifierExporter(arguments))
  def exportCustom(accept: String => Boolean)  (implicit arguments: Arguments) =
    optionalExporter(accept("exporter") || !arguments.report.notifier.isEmpty)(customExporter(arguments))

}
