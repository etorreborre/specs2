package org.specs2
package runner

import org.specs2.control.Throwablex._
import org.specs2.control._
import org.specs2.reporter.{NotifierPrinter, Notifier, TextPrinter}

import scala.reflect.ClassTag
import scalaz.effect._, IO._
import scalaz.{syntax, \/-, -\/}
import syntax.all._
import scalaz.std.vector._
import scalaz.std.list._
import scalaz.std.option._
import main.Arguments
import reflect.Classes
import reporter._, Printer._

/**
 * reusable actions for Runners
 */
object Runner {

  /**
   * Execute some actions and exit with the proper code if 'exist' is true
   */
  def execute(actions: Action[Unit], arguments: Arguments, exit: Boolean) = {
    val (warnings, result) = actions.run(consoleLogging).unsafePerformIO
    result.fold(
      ok => logUserWarnings(warnings)(consoleLogging) >> IO(exitSystem(0, exit)),
      error => error.fold(
        m      => logUserWarnings(warnings)(consoleLogging) >> consoleLogging(m),
        t      => logUserWarnings(warnings)(consoleLogging) >> logThrowable(t, arguments)(consoleLogging),
        (m, t) => logUserWarnings(warnings)(consoleLogging) >> consoleLogging(m) >> logThrowable(t, arguments)(consoleLogging)
      ) >> IO(exitSystem(100, exit))
    ).unsafePerformIO
  }

  /**
   * Use the console logging to log exceptions
   */
  def logThrowable(t: Throwable, arguments: Arguments)(print: String => IO[Unit]): IO[Unit] = {
    def logStack(exception: Throwable) =
      exception.chainedExceptions.traverse_(s => print("  caused by " + s.toString)) >>
      print("\nSTACKTRACE") >>
      exception.getStackTrace.toList.traverse_(e => print("  "+e.toString)) >>
      exception.chainedExceptions.traverse_ { s =>
        print("\n  CAUSED BY " + s.toString) >> s.getStackTrace.toList.traverse_(e => print("  "+e.toString))
      }

    if (!arguments.commandLine.boolOr("silent", false)) {
      t match {
        case UserException(m, throwable) =>
          print("\n"+m+"\n") >>
          logStack(throwable) >>
          print(" ")

        case ActionException(warnings, message, _) =>
          if (warnings.nonEmpty) print("Warnings:\n") >> print(warnings.mkString("", "\n", "\n")) else IO(()) >>
          message.traverseU(print).void

        case _: InterruptedException => print("User cancellation. Bye")

        case other =>
          print("\n"+t.toString+"\n") >>
          logStack(t)
          print("\n\nThis looks like a specs2 exception...\nPlease report it with the preceding stacktrace at http://github.com/etorreborre/specs2/issues") >>
          print(" ")

      }
    } else IO(())
  }

  /**
   * Log the issues which might have been caused by the user
   */
  def logUserWarnings(warnings: Vector[String])(print: String => IO[Unit]): IO[Unit] = {
    (if (warnings.nonEmpty) print("Warnings:\n") else IO(())) >>
    warnings.traverseU(print).void
  }

  /**
   * Exit the JVM with a given status
   */
  def exitSystem(status: Int, exit: Boolean) {
    if (exit) System.exit(status)
  }

  def createTextPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] = {
    if (!printerNames.map(_.name).exists(args.isSet) || args.isSet(CONSOLE.name)) Actions.ok(Some(TextPrinter))
    else noInstance("no console printer defined", args.verbose)
  }

  def createJUnitXmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createPrinterInstance(args, loader,
      JUNITXML, "org.specs2.reporter.JUnitXmlPrinter$",
      "cannot create a JUnit XML printer. Please check that specs2-junit.jar is on the classpath",
      "no JUnit XML printer defined")

  def createHtmlPrinter(args: Arguments, loader: ClassLoader): Action[Option[Printer]] =
    createPrinterInstance(args, loader,
      HTML, "org.specs2.reporter.HtmlPrinter$",
      "cannot create a HTML printer. Please check that specs2-html.jar is on the classpath",
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
    if (args.isSet(name.name))
      for {
        instance <- Classes.createInstanceEither[Printer](className, loader)
        result   <-
        instance match {
          case \/-(i) => Actions.ok(Some(i))
          case -\/(t) => noInstance(failureMessage, t, verbose = true)
        }
      } yield result
    else noInstance(noRequiredMessage, args.verbose)

  /** create a custom instance */
  def createCustomInstance[T <: AnyRef](args: Arguments, loader: ClassLoader,
                                        name: String, failureMessage: String => String, noRequiredMessage: String)(implicit m: ClassTag[T]): Action[Option[T]] =
    args.commandLine.value(name) match {
      case Some(className) =>
        for {
          instance <- Classes.createInstanceEither[T](className, loader)(m)
          result   <- instance match {
            case \/-(i) => Actions.ok(Some(i))
            case -\/(t) => noInstance(failureMessage(className), t, verbose = true)
          }
        } yield result

      case None => noInstance(noRequiredMessage, args.verbose)
    }

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String, t: Throwable, verbose: Boolean): Action[Option[T]] =
    log("", verbose)                 >>
    log(message, verbose)            >>
    log("", verbose)                 >>
    control.logThrowable(t, verbose) >>
    Actions.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String): Action[Option[T]] =
    log(message)      >>
      Actions.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String, verbose: Boolean): Action[Option[T]] =
    log(message, verbose) >>
      Actions.ok(None)

}

