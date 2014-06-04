package org.specs2
package reporter

import specification._
import scalaz.stream.Process
import scalaz.stream.io
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import scalaz.stream.Process.{Env => E, _}
import scalaz.concurrent.Task
import data.Processes._
import data.Fold
import execute._
import main.Arguments
import LogLine._
import text.Trim
import Trim._
import scalaz.concurrent.Task._
import reflect.Classes
import org.specs2.control._
import Actions._
import Fold._
import specification.core._
import specification.core._
import execute.FailureDetails
import specification.process._

/**
 * A Printer is essentially defined by a Fold that can run
 * a Process[Task, Fragment], use a Sink for side-effects and
 * accumulate state for final reporting
 */
trait Printer {
  def fold(env: Env, spec: SpecStructure): Fold[Fragment]

  /** convenience method to print a SpecStructure using the printer's Fold */
  def print(env: Env): SpecStructure => Task[Unit] = { spec: SpecStructure =>
    Fold.runFold(spec.contents, fold(env, spec))
  }
}

object Printer {
  val CONSOLE  = PrinterName("console")
  val HTML     = PrinterName("html")
  val JUNIT    = PrinterName("junit")
  val MARKDOWN = PrinterName("markdown")
  val JUNITXML = PrinterName("junitxml")
  val PRINTER  = PrinterName("printer")
  val NOTIFIER = PrinterName("notifier")

  val printerNames = Seq(CONSOLE, HTML, JUNIT, JUNITXML, MARKDOWN, PRINTER, NOTIFIER)

  case class PrinterName(name: String) extends AnyVal

  def createTextPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    if (!printerNames.map(_.name).exists(args.contains) || args.commandLine.isDefined(CONSOLE.name)) Actions.ok(Some(TextPrinter))
    else noPrinter("no console printer required", args.verbose)

  def createJUnitXmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    if (args.commandLine.isDefined(JUNITXML.name))
      Classes.createInstance[Printer]("org.specs2.reporter.JUnitXmlPrinter", loader).map(Some(_))
        .orElse(noPrinter("cannot create a JUnit XML printer. Please check that specs2-junit is on the classpath"))
    else
      noPrinter("no JUnit XML printer required", args.verbose)

  def createHtmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    if (args.commandLine.isDefined(HTML.name))
      Classes.createInstance[Printer]("org.specs2.reporter.HtmlPrinter", loader).map(Some(_))
        .orElse(noPrinter("cannot create a HTML printer. Please check that specs2-html is on the classpath"))
    else
      noPrinter("no HTML printer required", args.verbose)

  def createMarkdownPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    if (args.commandLine.isDefined(HTML.name))
      Classes.createInstance[Printer]("org.specs2.reporter.MarkdownPrinter", loader).map(Some(_))
        .orElse(noPrinter("cannot create a Markdown printer. Please check that specs2-markdown is on the classpath"))
    else
      noPrinter("no Markdown printer required", args.verbose)

  /** create a custom printer from a Name passed in arguments */
  def createPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    args.commandLine.value(PRINTER.name) match {
      case Some(className) =>
        Classes.createInstance[Printer](className, loader).map(Some(_))
          .orElse(noPrinter(s"cannot create a $className printer. Please check that this class can be instantiated"))
      case None =>
        noPrinter(s"no custom printer required", args.verbose)
    }

  def createNotifierPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    args.commandLine.value(NOTIFIER.name) match {
      case Some(className) =>
        Classes.createInstance[Notifier](className, loader).map(NotifierPrinter.printer).map(Some(_))
          .orElse(noPrinter(s"cannot create a $className notifier. Please check that this class can be instantiated"))

      case None =>
        noPrinter(s"no custome printer required", args.verbose)
    }

  private def noPrinter(message: String): Action[Option[Printer]] =
    log(message) >> Actions.ok(None)

  private def noPrinter(message: String, verbose: Boolean): Action[Option[Printer]] =
    log(message, verbose) >> Actions.ok(None)
}

