package org.specs2
package reporter

import reflect.Classes
import main.Arguments
import specification.{ExecutedSpecification, ExecutingSpecification}

private [specs2]
trait Exporters {
  type EE = ExecutingSpecification => ExecutedSpecification

  def isConsole(args: Arguments) = !Seq("html", "junitxml", "markdown").exists(args.contains) || args.contains("console")

  def exportAll(arguments: Arguments): EE = exportAll(arguments, (s: String) => arguments.commandLine.contains(s))

  def exportAll(args: Arguments, accept: String => Boolean): EE = (spec: ExecutingSpecification) => {
    exportAll(exporters(accept)(args))(args)(spec)
  }

  def exportAll(exporters: Seq[Exporting])(implicit arguments: Arguments): EE = (spec: ExecutingSpecification) => {
    val args = arguments.commandLineFilterNot("html", "markdown", "junitxml", "console", "notifier", "exporter")
    exporters.foreach(_.export(args)(spec))
    spec.executed
  }

  def exporters(implicit arguments: Arguments): Seq[Exporting] = exporters(arguments.contains _)

  def exporters(accept: String => Boolean)(implicit arguments: Arguments): Seq[Exporting] =
    Seq(exportHtml(accept),
        exportMarkdown(accept),
        exportJUnitxml(accept),
        exportNotifier(accept),
        exportCustom(accept),
        exportConsole(accept)).flatten

  def notifierExporter(arguments: Arguments): Option[Exporting] =
    Classes.createObject[Notifier](arguments.report.notifier, true).map(n => new NotifierExporting { val notifier = n })

  def customExporter(arguments: Arguments): Option[Exporting] = Classes.createObject[Exporter](arguments.report.exporter, true)

  protected def exporter(condition: Boolean)(e: =>Exporting) = if (condition) Some(e) else None
  protected def optionalExporter(condition: Boolean)(e: Option[Exporting]) = if (condition) e else None

  def exportHtml(accept: String => Boolean)    (implicit arguments: Arguments) = exporter(accept("html"))(Classes.createObject[Exporting]("org.specs2.reporter.HtmlExporting$", true).get)

  def exportMarkdown(accept: String => Boolean)  (implicit arguments: Arguments) = exporter(accept("markdown"))(Classes.createObject[Exporting]("org.specs2.reporter.MarkdownExporting$", true).get)

  def exportJUnitxml(accept: String => Boolean)(implicit arguments: Arguments) = exporter(accept("junitxml"))(Classes.createObject[Exporting]("org.specs2.reporter.JUnitXmlExporting$", true).get)

  def exportConsole(accept: String => Boolean) (implicit arguments: Arguments) = exporter(accept("console"))(Classes.createObject[Exporting]("org.specs2.reporter.TextExporting$", true).get)

  def exportNotifier(accept: String => Boolean)(implicit arguments: Arguments) =
    optionalExporter(accept("notifier") || !arguments.report.notifier.isEmpty)(notifierExporter(arguments))

  def exportCustom(accept: String => Boolean)  (implicit arguments: Arguments) =
    optionalExporter(accept("exporter") || !arguments.report.exporter.isEmpty)(customExporter(arguments))

}
