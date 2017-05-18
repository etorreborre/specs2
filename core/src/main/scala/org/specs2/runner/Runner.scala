package org.specs2
package runner

import control._, Throwablex._
import control.Operations._
import eff._, all._
import specification.core._
import specification.process.Stats
import scala.reflect.ClassTag
import org.specs2.fp._
import org.specs2.fp.syntax._
import main.Arguments
import reflect.Classes
import reporter._, Printer._

/**
 * reusable actions for Runners
 */
object Runner {

  /**
   * Execute some actions and exit with the proper code if 'exit' is true
   */
  def execute(actions: Action[Stats], arguments: Arguments, exit: Boolean)(env: Env): Unit = {
    val (result, warnings) = executeAction(actions, env.specs2ExecutionEnv, consoleLogging)
    val logging = (s: String) => Name(consoleLogging(s))
    result.fold(
      error => error.fold(
        t => logUserWarnings(warnings)(logging) >> logThrowable(t, arguments)(logging),
        m => logUserWarnings(warnings)(logging) >> logging(m)
      ) >> Name(exitSystem(100, exit)),
      ok => logUserWarnings(warnings)(logging) >>
        (if (ok.isSuccess) Name(exitSystem(0, exit)) else Name(exitSystem(1, exit)))
    ).value
  }

  /**
   * Use the console logging to log exceptions
   */
  def logThrowable(t: Throwable, arguments: Arguments)(print: String => Name[Unit]): Name[Unit] = {
    def logStack(exception: Throwable) =
      exception.chainedExceptions.traverse_(s => print("  caused by " + s.toString)) >>
        print("\nSTACKTRACE") >>
        exception.getStackTrace.toList.traverse_(e => print("  " + e.toString)) >>
        exception.chainedExceptions.traverse_ { s =>
          print("\n  CAUSED BY " + s.toString) >> s.getStackTrace.toList.traverse_(e => print("  " + e.toString))
        }

     def logException(m: String, throwable: Throwable) =
       print("\n" + m + "\n") >>
         logStack(throwable) >>
         print(" ")

    if (!arguments.commandLine.boolOr("silent", false)) {
      t match {
      case UserException(m, throwable) => logException(m, throwable)

      case ActionException(warnings, message, exception) =>
        if (warnings.nonEmpty) print("Warnings:\n") >> print(warnings.mkString("", "\n", "\n"))
        else Name(()) >>
          message.traverse(print).void >>
          exception.traverse(e => logException(e.getMessage, e)).void

      case _: InterruptedException => print("User cancellation. Bye")

      case other =>
        print("\n" + t.toString + "\n") >>
          logStack(t) >>
        print("\n\nThis looks like a specs2 exception...\nPlease report it with the preceding stacktrace at http://github.com/etorreborre/specs2/issues") >>
          print(" ")

      }
    } else Name(())
  }

  /**
   * Log the issues which might have been caused by the user
   */
  def logUserWarnings(warnings: List[String])(print: String => Name[Unit]): Name[Unit] = {
    (if (warnings.nonEmpty) print("Warnings:\n") else Name(())) >>
      warnings.traverse(print).void
  }

  /**
   * Run a spec structure and its linked specifications if "all" is set
   * on the command line
   */
  def runSpecStructure(specStructure: SpecStructure, env: Env, loader: ClassLoader, printers: List[Printer]): Action[Stats] = {
    val arguments = env.arguments

    val report: Action[Stats] =
      if (arguments.isSet("all")) {
        for {
          reporter <- ClassRunner.createReporter(arguments, loader).toAction
          ss       <- SpecStructure.linkedSpecifications(specStructure, env, loader).toAction
          sorted   <- delayed(SpecStructure.topologicalSort(ss)(env.executionEnv).getOrElse(ss)).toAction
          _        <- reporter.prepare(env, printers)(sorted.toList)
          stats    <- sorted.toList.map(Reporter.report(env, printers)).sequence
          _        <- Reporter.finalize(env, printers)(sorted.toList)
        } yield stats.foldMap(identity _)

      } else Reporter.report(env, printers)(specStructure)

    report
  }

  /**
   * Exit the JVM with a given status
   */
  def exitSystem(status: Int, exit: Boolean) {
    if (exit) System.exit(status)
  }

  def createTextPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] = {
    if (!printerNames.map(_.name).exists(args.isSet) || args.isSet(CONSOLE.name)) Operations.ok(Some(TextPrinter))
    else noInstance("no console printer defined", args.verbose)
  }

  def createJUnitXmlPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] =
    createPrinterInstance(args, loader,
      JUNITXML, "org.specs2.reporter.JUnitXmlPrinter$",
      "cannot create a JUnit XML printer. Please check that specs2-junit.jar is on the classpath",
      "no JUnit XML printer defined")

  def createHtmlPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] =
    createPrinterInstance(args, loader,
      HTML, "org.specs2.reporter.HtmlPrinter$",
      "cannot create a HTML printer. Please check that specs2-html.jar is on the classpath",
      "no HTML printer defined")

  def createMarkdownPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] =
    createPrinterInstance(args, loader,
      MARKDOWN, "org.specs2.reporter.MarkdownPrinter$",
      "cannot create a Markdown printer. Please check that specs2-markdown is on the classpath",
      "no Markdown printer defined")

  /** create a custom printer from a Name passed in arguments */
  def createPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] =
    createCustomInstance[Printer](args, loader,
      PRINTER.name,
      (className: String) => s"cannot create a $className printer. Please check that this class can be instantiated",
      s"no custom printer defined")

  /** create a custom printer from a Notifier instance passed in arguments */
  def createNotifierPrinter(args: Arguments, loader: ClassLoader): Operation[Option[Printer]] =
    createCustomInstance[Notifier](args, loader,
      NOTIFIER.name,
      (className: String) => s"cannot create a $className notifier. Please check that this class can be instantiated",
      s"no custom notifier defined").map(_.map(NotifierPrinter.printer))

  /** create a built-in specs2 printer */
  def createPrinterInstance(args: Arguments, loader: ClassLoader, name: PrinterName, className: String, failureMessage: String, noRequiredMessage: String): Operation[Option[Printer]] =
    if (args.isSet(name.name))
      for {
        instance <- Classes.createInstanceEither[Printer](className, loader)
        result <-
          instance match {
            case Right(i) => Operations.ok(Some(i))
            case Left(t) => noInstance(failureMessage, t, verbose = true)
          }
      } yield result
    else noInstance(noRequiredMessage, args.verbose)

  /** create a custom instance */
  def createCustomInstance[T <: AnyRef](args: Arguments, loader: ClassLoader,
    name: String, failureMessage: String => String, noRequiredMessage: String)(implicit m: ClassTag[T]): Operation[Option[T]] =
    args.commandLine.value(name) match {
    case Some(className) =>
      for {
        instance <- Classes.createInstanceEither[T](className, loader)(m)
        result <-
          instance match {
            case Right(i) => Operations.ok(Some(i))
            case Left(t) => noInstance(failureMessage(className), t, verbose = true)
          }
      } yield result

    case None => noInstance(noRequiredMessage, args.verbose)
    }

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String, t: Throwable, verbose: Boolean): Operation[Option[T]] =
    log("", verbose) >>
      log(message, verbose) >>
      log("", verbose) >>
      ConsoleEffect.logThrowable(t, verbose) >>
      Operations.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String): Operation[Option[T]] =
    log(message) >>
      Operations.ok(None)

  /** print a message if a class can not be instantiated */
  def noInstance[T](message: String, verbose: Boolean): Operation[Option[T]] =
    log(message, verbose) >>
      Operations.ok(None)

}
