package org.specs2
package reporter

import reflect.Classes
import main.Arguments
import specification.{ExecutedSpecification, ExecutingSpecification}

private [specs2]
trait Exporters {
  type EE = ExecutingSpecification => ExecutedSpecification

  def isConsole(args: Arguments) = !Seq("html", "junitxml", "markup").exists(args.contains) || args.contains("console")

  def exportAll(arguments: Arguments): EE = exportAll(arguments, (s: String) => arguments.commandLine.contains(s))

  def exportAll(args: Arguments, accept: String => Boolean): EE = (spec: ExecutingSpecification) => {
    exportAll(exporters(accept)(args))(args)(spec)
  }

  def exportAll(exporters: Seq[Exporting])(implicit arguments: Arguments): EE = (spec: ExecutingSpecification) => {
    val args = arguments.commandLineFilterNot("html", "markup", "junitxml", "console", "notifier", "exporter")
    exporters.foreach(_.export(args)(spec))
    spec.executed
  }

  def exporters(implicit arguments: Arguments): Seq[Exporting] = exporters(arguments.contains _)

  def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] =
    Seq(exportHtml(accept),
        exportMarkup(accept),
        exportJUnitxml(accept),
        exportNotifier(accept),
        exportCustom(accept),
        exportConsole(accept)).flatten

  def notifierExporter(arguments: Arguments): Option[Exporting] =
    Classes.createObject[Notifier](arguments.report.notifier, true).map(n => new NotifierExporting { val notifier = n })

  def customExporter(arguments: Arguments): Option[Exporting] = Classes.createObject[Exporter](arguments.report.exporter, true)

  protected def exporter(condition: Boolean)(e: =>Exporting) = if (condition) Some(e) else None
  protected def optionalExporter(condition: Boolean)(e: Option[Exporting]) = if (condition) e else None

  def exportHtml(accept: String => Boolean)    (implicit arguments: Arguments) = exporter(accept("html"))(HtmlExporting)

  def exportMarkup(accept: String => Boolean)  (implicit arguments: Arguments) = exporter(accept("markup"))(MarkupExporting)

  def exportJUnitxml(accept: String => Boolean)(implicit arguments: Arguments) = exporter(accept("junitxml"))(JUnitXmlExporting)

  def exportConsole(accept: String => Boolean) (implicit arguments: Arguments) = exporter(accept("console"))(TextExporting)

  def exportNotifier(accept: String => Boolean)(implicit arguments: Arguments) =
    optionalExporter(accept("notifier") || !arguments.report.notifier.isEmpty)(notifierExporter(arguments))

  def exportCustom(accept: String => Boolean)  (implicit arguments: Arguments) =
    optionalExporter(accept("exporter") || !arguments.report.exporter.isEmpty)(customExporter(arguments))

}
