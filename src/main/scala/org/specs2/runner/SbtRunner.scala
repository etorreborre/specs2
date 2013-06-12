package org.specs2
package runner

import _root_.org.scalasbt.testing._
import main.Arguments
import control.Throwablex._
import reporter._
import specification._
import control.Exceptions._
import specification.ExecutedSpecification
import reflect.Classes._
import io.ConsoleOutput
import internal.scalaz.Scalaz._
import Fingerprints._

/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class Specs2Framework extends Framework {
  def name = "specs2"
  def fingerprints = Array[Fingerprint](fp1, fp2)
  def runner(args: Array[String], loader: ClassLoader, handler: EventHandler, loggers: Array[Logger]) = new SbtRunner(args, loader, handler, loggers)
}

object Fingerprints {
  val fp1 =  new SpecificationFingerprint { }
  val fp2 =  new FilesRunnerFingerprint { }
}

trait SpecificationFingerprint extends SubclassFingerprint {
  def isModule = true
  def superclassName = "org.specs2.specification.SpecificationStructure"
}
trait FilesRunnerFingerprint extends SubclassFingerprint {
  def isModule = true
  def superclassName = "org.specs2.runner.FilesRunner"
}

case class SbtRunner(args: Array[String],
                     loader: ClassLoader,
                     handler: EventHandler,
                     loggers: Array[Logger]) extends _root_.org.scalasbt.testing.Runner with Events with SbtLoggers with Exporters {

  private implicit val commandLineArguments = Arguments(args:_*)

  def task(className: String, fingerprint: Fingerprint) = new Task {
    def tags = Array[String]()
    def execute = {
      fingerprint match {
        case f: SpecificationFingerprint => specificationRun(className)
        case f: FilesRunnerFingerprint   => filesRun(className)
        case _                           => ()
      }
      Array[Task]()
    }
  }

  def task(className: String, isModule: Boolean, selectors: Array[Selector]) = new Task {
    def tags = Array[String]()
    def execute = {
      specificationRun(className)
      Array[Task]()
    }
  }

  def done = false

  def specificationRun(className: String) =
    SpecificationStructure.createSpecificationEither(className, loader) match {
      case Left(e)  => handleClassCreationError(className, e)
      case Right(s) => reporter(className, handler)(args).report(s)(s.content.arguments.overrideWith(commandLineArguments))
    }

  def filesRun(className: String) =
    toRun[FilesRunner](className).right.toOption.toSeq.flatMap(_.run(args)).flatMap(_.issues).foreach { issue =>
      handler.handle(result(className, issue.result))
    }

  private def toRun[T <: AnyRef : Manifest](className: String): Either[Throwable, T] = {
    val runner: Either[Throwable, T] = create[T](className + "$", loader) match {
      case Right(s) => Right(s)
      case Left(e) => create[T](className, loader)
    }
    runner.left.map { e => handleClassCreationError(className, e) }
    runner
  }

  /**
   * Notify sbt that the specification could not be created
   */
  private def handleClassCreationError(className: String, e: Throwable) {
    handler.handle(error(className, e))
    logError("Could not create an instance of "+className+"\n")
    (e :: e.chainedExceptions) foreach { s =>
      logError("  caused by " + s.toString)
      s.getStackTrace.foreach(t => logError("  " + t.toString))
    }
  }

  protected def reporter(className: String, eventHandler: EventHandler)(args: Array[String]) =
    new SbtConsoleReporter(consoleExporter(className, args, eventHandler), (a: Arguments) => otherExporters(className, args, eventHandler)(a))

  /** @return true if the console must report the results */
  private def isConsole(args: Array[String]) = !Seq("html", "junitxml", "markup").exists(args.contains) || args.contains("console")
  private def consoleExporter(className: String, args: Array[String], handler: EventHandler) =
    exporter(isConsole(args))(new SbtExporter(className, handler, loggers))

  protected def finalExporter(className: String, handler: EventHandler) = FinalResultsExporter(className, handler, loggers)

  def otherExporters(className: String, args: Array[String], handler: EventHandler)(implicit arguments: Arguments): Seq[Exporting] = {
    val exportFinalStats = exporter(!isConsole(args))(finalExporter(className, handler))
    super.exporters((args.filterNot(_ == "console")).contains)(arguments) ++ exportFinalStats.toSeq
  }

  /** @return the list of all the exporters depending on the arguments passed on the command line */
  def exporters(className: String, args: Array[String], handler: EventHandler)(implicit arguments: Arguments): Seq[Exporting] = {
    consoleExporter(className, args, handler).toSeq ++ otherExporters(className, args, handler)(arguments)
  }
}

/**
 * This object can be used to debug the behavior of the SbtRunner
 */
object sbtRunner extends SbtRunner(Array[String](), Thread.currentThread().getContextClassLoader, NoEventHandler, Array(ConsoleLogger)) with SystemExit with ConsoleOutput {
  def main(arguments: Array[String]) {
    exitSystem(start(arguments:_*))
  }

  protected val errorHandler = (e: Throwable) => e match { case e =>
    println("\nAn error occurred. " +
      "Please create an issue on the http://specs2.org website with the stacktrace below. Thanks.")
    e.printStackTrace
  }

  def start(arguments: String*): Option[ExecutedSpecification] = {
    if (arguments.length == 0)
      println("The first argument should at least be the specification class name")
    implicit val commandLineArgs = Arguments(arguments.drop(1):_*)
    val className = arguments(0)
    val sbtReporter = reporter(className, NoEventHandler)(arguments.toArray)
    execute(sbtReporter, createSpecification(className)).headOption
  }

  private def execute(sbtReporter: Reporter, specification: SpecificationStructure)(implicit args: Arguments = Arguments()): Option[ExecutedSpecification] = {
    tryo(sbtReporter.report(specification)(args.overrideWith(specification.content.arguments)))(errorHandler)
  }

  private def createSpecification(className: String)(implicit args: Arguments) = SpecificationStructure.createSpecification(className, loader)
}
object NoEventHandler extends EventHandler {
  def handle(event: Event) {}
}

object ConsoleLogger extends Logger {
  def ansiCodesSupported = false
  def error(message: String) = println("error: " + message)
  def info(message: String)  = println("info: " + message)
  def warn(message: String)  = println("warn: " + message)
  def debug(message: String) = println("debug: " + message)
  def trace(t: Throwable)    = println("trace: " + t)
}