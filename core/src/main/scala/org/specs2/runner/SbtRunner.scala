package org.specs2
package runner

import _root_.sbt.testing._
import main.Arguments
import control.Throwablex._
import reporter._
import specification._
import control.Exceptions._
import specification.ExecutedSpecification
import reflect.Classes._
import io.ConsoleOutput
import scalaz.Scalaz._
import Fingerprints._
import org.specs2.reflect.Classes

/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class Specs2Framework extends Framework {
  def name = "specs2"
  def fingerprints = Array[Fingerprint](fp1, fp1m, fp2, fp2m)
  def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) =
    new SbtRunner(args, remoteArgs, loader)
}

object Fingerprints {
  val fp1  =  new SpecificationFingerprint { override def toString = "specs2 Specification fingerprint" }
  val fp1m =  new SpecificationFingerprint { override def toString = "specs2 Specification fingerprint"; override def isModule = false }
  val fp2  =  new FilesRunnerFingerprint   { override def toString = "specs2 Specification files fingerprint"}
  val fp2m =  new FilesRunnerFingerprint   { override def toString = "specs2 Specification files fingerprint"; override def isModule = false }
}

trait SpecificationFingerprint extends SubclassFingerprint {
  def isModule = true
  def superclassName = "org.specs2.specification.SpecificationStructure"
  def requireNoArgConstructor = false
}
trait FilesRunnerFingerprint extends SubclassFingerprint {
  def isModule = true
  def superclassName = "org.specs2.runner.FilesRunner"
  def requireNoArgConstructor = false
}
case class SbtRunner(args: Array[String],
                     remoteArgs: Array[String],
                     loader: ClassLoader) extends _root_.sbt.testing.Runner with Events with SbtLoggers with Exporters {

  private implicit val commandLineArguments = Arguments(args:_*)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = taskDefs.map(newTask)

  def newTask = (aTaskDef: TaskDef) =>
    new Task {
      def tags = Array[String]()
      def execute(handler: EventHandler, loggers: Array[Logger]) = {
        taskDef.fingerprint match {
          case f: SubclassFingerprint    =>
            if (f.superclassName.endsWith("SpecificationStructure")) specificationRun(aTaskDef, loader, handler, loggers)
            else if (f.superclassName.endsWith("FilesRunner"))       filesRun(aTaskDef, args, loader, handler, loggers)
            else                                                     ()
          case _                         => ()
        }
        // nothing more to execute
        Array[Task]()
      }
      def taskDef = aTaskDef
    }

  def done = ""

  private def specificationRun(taskDef: TaskDef, loader: ClassLoader, handler: EventHandler, loggers: Array[Logger]) = {
    SpecificationStructure.createSpecificationEither(taskDef.fullyQualifiedName, loader) match {
      case Left(e)  => handleClassCreationError(taskDef, e, handler, loggers)
      case Right(s) => reporter(taskDef, handler, loggers)(args).report(s)(s.content.arguments.overrideWith(commandLineArguments))
    }
  }

  def filesRun(taskDef: TaskDef, args: Array[String], loader: ClassLoader, handler: EventHandler, loggers: Array[Logger]) =
    toRun[FilesRunner](taskDef, loader, handler, loggers).right.toOption.toSeq.flatMap(_.run(args)).flatMap(_.issues).foreach { issue =>
      handler.handle(result(taskDef)(issue.result))
    }

  private def toRun[T <: AnyRef : Manifest](taskDef: TaskDef, loader: ClassLoader, handler: EventHandler, loggers: Array[Logger]): Either[Throwable, T] = {
    val runner: Either[Throwable, T] = create[T](taskDef.fullyQualifiedName + "$", loader) match {
      case Right(s) => Right(s)
      case Left(e) => create[T](taskDef.fullyQualifiedName, loader)
    }
    runner.left.map { e => handleClassCreationError(taskDef, e, handler, loggers) }
    runner
  }

  /**
   * Notify sbt that the specification could not be created
   */
  private def handleClassCreationError(taskDef: TaskDef, e: Throwable, handler: EventHandler, loggers: Array[Logger]) {
    def logerror(m: String) = logError(loggers)(m)

    handler.handle(error(taskDef, e))

    logerror("\nCould not create an instance of "+taskDef.fullyQualifiedName+"\n")

    logerror("\n"+e.toString+"\n")
    e.chainedExceptions.foreach(s => logerror("  caused by "+s.toString))

    logerror("\nSTACKTRACE")
    e.getStackTrace.foreach(t => logerror("  "+t.toString))

    e.chainedExceptions.foreach { s =>
      logerror("\n  CAUSED BY "+s.toString)
      s.getStackTrace.foreach(t => logerror("  "+t.toString))
    }

  }

  protected def reporter(taskDef: TaskDef, handler: EventHandler, loggers: Array[Logger])(args: Array[String]) =
    new SbtConsoleReporter(consoleExporter(taskDef, args, handler, loggers),
                           (a: Arguments) => otherExporters(taskDef, args, handler, loggers)(a))

  /** @return true if the console must report the results */
  private def consoleExporter(taskDef: TaskDef, args: Array[String], handler: EventHandler, loggers: Array[Logger]) =
    exporter(isConsole(Arguments(args:_*)))(new SbtExporter(taskDef, handler, loggers))

  protected def finalExporter(taskDef: TaskDef, handler: EventHandler, loggers: Array[Logger]) = FinalResultsExporter(taskDef, handler, loggers)

  def otherExporters(taskDef: TaskDef, args: Array[String], handler: EventHandler, loggers: Array[Logger])(implicit arguments: Arguments): Seq[Exporting] = {
    val exportFinalStats = exporter(!isConsole(Arguments(args:_*)))(finalExporter(taskDef, handler, loggers))
    super.exporters(args.filterNot(_ == "console").contains(_))(arguments) ++ exportFinalStats.toSeq
  }

  /** @return the list of all the exporters depending on the arguments passed on the command line */
  def exporters(taskDef: TaskDef, args: Array[String], handler: EventHandler, loggers: Array[Logger])(implicit arguments: Arguments): Seq[Exporting] = {
    consoleExporter(taskDef, args, handler, loggers).toSeq ++ otherExporters(taskDef, args, handler, loggers)(arguments)
  }
}

/**
 * This object can be used to debug the behavior of the SbtRunner
 */
object sbtRun extends SbtRunner(Array[String](), Array[String](), Thread.currentThread().getContextClassLoader) with SystemExit with ConsoleOutput {
  def main(arguments: Array[String]) {
    exitSystem(start(arguments:_*))
  }

  def start(arguments: String*): Option[ExecutedSpecification] = {
    if (arguments.length == 0)
      println("The first argument should at least be the specification class name")
    implicit val commandLineArgs = Arguments(arguments.drop(1):_*)
    val className = arguments(0)
    val sbtReporter = reporter(new TaskDef(className, Fingerprints.fp1, true, Array[Selector]()), NoEventHandler, Array(ConsoleLogger))(arguments.toArray)
    execute(sbtReporter, createSpecification(className)).headOption
  }

  private def execute(sbtReporter: Reporter, specification: SpecificationStructure)(implicit args: Arguments = Arguments()): Option[ExecutedSpecification] = {
    tryo(sbtReporter.report(specification)(args.overrideWith(specification.content.arguments)))(ClassRunner.errorHandler)
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