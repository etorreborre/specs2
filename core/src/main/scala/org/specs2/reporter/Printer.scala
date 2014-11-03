package org.specs2
package reporter

import scalaz.{-\/, \/-}
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import scalaz.concurrent.Task
import data.Fold
import main.Arguments
import reflect.Classes
import org.specs2.control._
import specification.core._
import runner.Runner._
/**
 * A Printer is essentially defined by a Fold that:
 *
 *  - can run a Process[Task, Fragment]
 *  - uses a Sink for side-effects and
 *  - accumulates state for final reporting
 */
trait Printer {
  def prepare(env: Env, specifications: List[SpecificationStructure]): Action[Unit]
  def finalize(env: Env, specifications: List[SpecificationStructure]): Action[Unit]

  def fold(env: Env, spec: SpecStructure): Fold[Fragment]

  /** convenience method to print a SpecStructure using the printer's Fold */
  def print(env: Env): SpecStructure => Task[Unit] = { spec: SpecStructure =>
    Fold.runFold(spec.contents, fold(env, spec))
  }
}

/**
 * specs2 built-in printers and creation methods based on the command line arguments
 */
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
    else noInstance("no console printer defined", args.verbose)

  def createJUnitXmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createPrinterInstance(args, loader,
      JUNITXML, "org.specs2.reporter.JUnitXmlPrinter$",
      "cannot create a JUnit XML printer. Please check that specs2-junit is on the classpath",
      "no JUnit XML printer defined")

  def createHtmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createPrinterInstance(args, loader,
      HTML, "org.specs2.reporter.HtmlPrinter$",
      "cannot create a HTML printer. Please check that specs2-html is on the classpath",
      "no HTML printer defined")

  def createMarkdownPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createPrinterInstance(args, loader,
      MARKDOWN, "org.specs2.reporter.MarkdownPrinter$",
      "cannot create a Markdown printer. Please check that specs2-markdown is on the classpath",
      "no Markdown printer defined")

  /** create a custom printer from a Name passed in arguments */
  def createPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createCustomInstance[Printer](args, loader,
     PRINTER.name,
      (className: String) => s"cannot create a $className printer. Please check that this class can be instantiated",
      s"no custom printer defined")

  /** create a custom printer from a Notifier instance passed in arguments */
  def createNotifierPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createCustomInstance[Notifier](args, loader,
      NOTIFIER.name,
      (className: String) => s"cannot create a $className notifier. Please check that this class can be instantiated",
      s"no custom notifier defined").map(_.map(NotifierPrinter.printer))

  /** create a built-in specs2 printer */
  def createPrinterInstance(args: Arguments, loader: ClassLoader, name: PrinterName, className: String, failureMessage: String, noRequiredMessage: String): Action[Option[Printer]] =
    if (args.commandLine.isDefined(name.name))
      for {
        instance <- Classes.createInstanceEither[Printer](className, loader)
        result   <-
          instance match {
            case \/-(i) => Actions.ok(Some(i))
            case -\/(t) => noInstance(failureMessage, t, args.verbose)
          }
      } yield result
    else noInstance(noRequiredMessage, args.verbose)

}

