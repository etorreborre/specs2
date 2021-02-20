package org.specs2
package reporter

import control.*
import main.*
import fp.syntax.*
import Printer.*
import specification.core.*

/**
 * Create printers based on their class names and the arguments passed by the user
 * Note that this operation might fail if the corresponding printer classes are not on the class path
 */
case class PrinterFactory(arguments: Arguments, customInstances: CustomInstances, logger: Logger):

  /** accepted printers created from the list of arguments */
  def createPrinters: Operation[List[Printer]] =
    List(createTextPrinter,
      createJUnitXmlPrinter,
      createHtmlPrinter,
      createMarkdownPrinter,
      createPrinter,
      createNotifierPrinter).sequence.map(_.flatten)

  def createTextPrinter: Operation[Option[Printer]] =
    if !printerNames.map(_.name).exists(arguments.isSet) || arguments.isSet(CONSOLE.name) then Operation.ok(Some(TextPrinter.default))
    else customInstances.noInstance[Printer]("no console printer defined")

  def createJUnitXmlPrinter: Operation[Option[Printer]] =
   customInstances.createPrinterInstance(
      JUNITXML, "org.specs2.reporter.JUnitXmlPrinter$",
      "cannot create a JUnit XML printer. Please check that specs2-junit.jar is on the classpath",
      "no JUnit XML printer defined")

  def createHtmlPrinter: Operation[Option[Printer]] =
    customInstances.createPrinterInstance(
      HTML, "org.specs2.reporter.HtmlPrinter$",
      "cannot create a HTML printer. Please check that specs2-html.jar is on the classpath",
      "no HTML printer defined")

  def createMarkdownPrinter: Operation[Option[Printer]] =
    customInstances.createPrinterInstance(
      MARKDOWN, "org.specs2.reporter.MarkdownPrinter$",
      "cannot create a Markdown printer. Please check that specs2-markdown is on the classpath",
      "no Markdown printer defined")

  /** create a custom printer from a Name passed in arguments */
  def createPrinter: Operation[Option[Printer]] =
    customInstances.createCustomInstance[Printer](
      PRINTER.name,
      (className: String) => s"cannot create a $className printer. Please check that this class can be instantiated",
      s"no custom printer defined")

  /** create a custom printer from a Notifier instance passed in arguments */
  def createNotifierPrinter: Operation[Option[Printer]] =
    customInstances.createCustomInstance[Notifier](
      NOTIFIER.name,
      (className: String) => s"cannot create a $className notifier. Please check that this class can be instantiated",
      s"no custom notifier defined").map(_.map(NotifierPrinter(arguments).printer))


object PrinterFactory:

  def default: PrinterFactory =
    create(EnvDefault.default)

  def create(env: Env): PrinterFactory =
    PrinterFactory(env.arguments, CustomInstances.create(env.arguments, env.systemLogger), env.systemLogger)

