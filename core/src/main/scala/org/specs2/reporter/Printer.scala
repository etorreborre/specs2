package org.specs2
package reporter

import scala.reflect.ClassTag
import scalaz.{-\/, \/-}
import scalaz.std.anyVal._
import scalaz.syntax.bind._
import scalaz.concurrent.Task
import data.Fold
import main.Arguments
import reflect.Classes
import org.specs2.control._
import specification.core._

/**
 * A Printer is essentially defined by a Fold that:
 *
 *  - can run a Process[Task, Fragment]
 *  - uses a Sink for side-effects and
 *  - accumulates state for final reporting
 */
trait Printer {
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
    else noPrinter("no console printer defined", args.verbose)

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
    createCustomPrinterInstance[Printer](args, loader,
     PRINTER,
      (className: String) => s"cannot create a $className printer. Please check that this class can be instantiated",
      s"no custom printer defined")

  /** create a custom printer from a Notifier instance passed in arguments */
  def createNotifierPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createCustomPrinterInstance[Notifier](args, loader,
      NOTIFIER,
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
            case -\/(t) => noPrinter(failureMessage, t, args.verbose)
          }
      } yield result
    else noPrinter(noRequiredMessage, args.verbose)

  /** create a custom specs2 printer */
  def createCustomPrinterInstance[T <: AnyRef](args: Arguments, loader: ClassLoader,
                                               name: PrinterName, failureMessage: String => String, noRequiredMessage: String)(implicit m: ClassTag[T]): Action[Option[T]] =
    args.commandLine.value(name.name) match {
      case Some(className) =>
        for {
          instance <- Classes.createInstanceEither[T](className, loader)(m)
          result   <- instance match {
            case \/-(i) => Actions.ok(Some(i))
            case -\/(t) => noPrinter(failureMessage(className), t, args.verbose)
          }
        } yield result

      case None => noPrinter(noRequiredMessage, args.verbose)
    }

  /** print a message if a printer can not be instantiated */
  def noPrinter[T](message: String, t: Throwable, verbose: Boolean): Action[Option[T]] =
    log("", verbose)         >>
    log(message, verbose)    >>
    log("", verbose)         >>
    logThrowable(t, verbose) >>
    Actions.ok(None)

  /** print a message if a printer can not be instantiated */
  def noPrinter[T](message: String): Action[Option[T]] =
    log(message)      >>
    Actions.ok(None)

  /** print a message if a printer can not be instantiated */
  def noPrinter[T](message: String, verbose: Boolean): Action[Option[T]] =
    log(message, verbose) >>
    Actions.ok(None)
}

