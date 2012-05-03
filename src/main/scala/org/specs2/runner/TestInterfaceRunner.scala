package org.specs2
package runner

import _root_.org.scalatools.testing._
import main.Arguments
import control.Throwablex._
import reporter._
import specification._
import Fingerprints._

/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class SpecsFramework extends Framework {
  def name = "specs2"
  def tests = Array[Fingerprint](fp1, fp2, fp3, fp4)
  def testRunner(classLoader: ClassLoader, loggers: Array[Logger]) = new TestInterfaceRunner(classLoader, loggers)
}

object Fingerprints {
  val fp1 =  new Specs2Fingerprint { def isModule = false }
  val fp2 =  new Specs2Fingerprint { def isModule = true  }
  val fp3 =  new FilesRunnerFingerprint { def isModule = false }
  val fp4 =  new FilesRunnerFingerprint { def isModule = true  }
}

trait Specs2Fingerprint extends TestFingerprint {
  def superClassName = "org.specs2.specification.SpecificationStructure"
}
trait FilesRunnerFingerprint extends TestFingerprint {
  def superClassName = "org.specs2.runner.FilesRunner"
}

/**
 * Runner for TestInterface.
 * It creates a Specification class with the given classLoader the classes which can be executed by the specs2 library.
 * 
 * Then it uses a NotifierRunner to notify the EventHandler of the test events.
 */
class TestInterfaceRunner(loader: ClassLoader, val loggers: Array[Logger]) extends _root_.org.scalatools.testing.Runner 
  with HandlerEvents with TestLoggers with Exporters {
  import reflect.Classes._

  def run(className: String, fingerprint: TestFingerprint, handler: EventHandler, args: Array[String]) =
    fingerprint match {
      case f if f.superClassName == fp3.superClassName => runFilesRunner(className, handler, args)
      case other                                       => runSpecification(className, handler, args)
    }

  def runSpecification(className: String, handler: EventHandler, args: Array[String]): Any = {
    implicit val commandLineArguments = Arguments(args:_*)
    SpecificationStructure.createSpecificationEither(className, loader) match {
      case Left(e)  => handleClassCreationError(className, handler, e)
      case Right(s) => reporter(handler)(args).report(s)(s.content.arguments.overrideWith(commandLineArguments))
    }
  }
  
  def runFilesRunner(className: String, handler: EventHandler, args: Array[String]) {
    toRun[FilesRunner](className, handler).right.toOption.toSeq.flatMap(_.run(args)).flatMap(_.issues).foreach { issue =>
      handler.handle(result(issue.result))
    }
  }

  private def toRun[T <: AnyRef : Manifest](className: String, handler: EventHandler): Either[Throwable, T] = {
    val runner: Either[Throwable, T] = create[T](className + "$", loader) match {
      case Right(s) => Right(s)
      case Left(e) => create[T](className, loader)
    }
    runner.left.map { e => handleClassCreationError(className, handler, e) }
    runner
  }

  /**
   * Notify sbt that the specification could not be created
   */
  private def handleClassCreationError(className: String, handler: EventHandler, e: Throwable) {
    handler.handle(error(className, e))
    logError("Could not create an instance of "+className+"\n")
    (e :: e.chainedExceptions) foreach { s =>
      logError("  caused by " + s.toString)
      s.getStackTrace.foreach(t => logError("  " + t.toString))
    }
  }

  protected def reporter(handler: EventHandler)(args: Array[String]): Reporter = new ConsoleReporter {
    override def export(implicit arguments: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
      exportToOthers(exporters(args, handler))(arguments <| spec.arguments)(spec)
      spec.executed
    }
  }

  protected def finalExporter(handler: EventHandler) = FinalResultsReporter(handler, loggers)

  def exporters(args: Array[String], handler: EventHandler)(implicit arguments: Arguments): Seq[Exporting] = {
    val isConsole = !Seq("html", "junitxml").exists(args.contains) || args.contains("console")

    def console          = exporter(isConsole)(new TestInterfaceReporter(handler, loggers))
    def exportFinalStats = exporter(!isConsole)(finalExporter(handler))
    super.exporters((args.filterNot(_ == "console")).contains) ++ Seq(console, exportFinalStats).flatten
  }
}
/**
 * This reporter will just notify the test interface about test results for the end statistics
 *
 * It is only used if we are not using the Console exporter
 */
case class FinalResultsReporter(override val handler: EventHandler,
                                override val loggers: Array[Logger]) extends TestInterfaceReporter(handler, loggers) {
  override def export(implicit args: Arguments): ExecutingSpecification => ExecutedSpecification = (spec: ExecutingSpecification) => {
    val executed = spec.execute
    executed.fragments foreach handleFragment(args)
    executed
  }
}

