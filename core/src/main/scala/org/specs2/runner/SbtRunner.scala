package org.specs2
package runner

import Runner._
import specification.process.Stats
import sbt.testing._
import Fingerprints._
import main._
import reporter._
import control.{Logger => _, _}
import Actions._
import scalaz._, Scalaz._
import scalaz.effect.IO
import reporter.SbtLineLogger
import reflect.Classes
import reporter.Printer._
import specification.core._
import eff._
import ErrorEffect._

/**
 * Runner for Sbt
 */
case class SbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) extends _root_.sbt.testing.Runner {
  private lazy val commandLineArguments = Arguments(args: _*)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.map(newTask).toArray

  def newTask = (aTaskDef: TaskDef) =>
    new Task {
      def tags = Array[String]()

      def execute(handler: EventHandler, loggers: Array[Logger]) = {
        taskDef.fingerprint match {
        case f: SubclassFingerprint =>
          if (f.superclassName.endsWith("SpecificationStructure")) {
            val action = specificationRun(aTaskDef, loader, handler, loggers, isModule = f.isModule)
            val (result, warnings) = executeAction(action, consoleLogging)
            result.fold(
              e => {
                if (warnings.nonEmpty) handleRunWarnings(warnings, loggers, commandLineArguments)
                else handleRunError(e, loggers, sbtEvents(taskDef, handler), commandLineArguments)
              },
              _ => handleRunWarnings(warnings, loggers, commandLineArguments)
            )
          }
          else ()
        case _ => ()
        }
        // nothing more to execute
        Array[Task]()
      }

      def taskDef = aTaskDef
    }

  def done = ""

  def specificationRun(taskDef: TaskDef, loader: ClassLoader, handler: EventHandler, loggers: Array[Logger], isModule: Boolean): Action[Stats] = {
    val env = Env(arguments = commandLineArguments)
    Classes.createInstance[SpecificationStructure](taskDef.fullyQualifiedName + (if (isModule) "$" else ""), loader, env.defaultInstances).flatMap { spec =>
      val report: Action[Stats] =
        if (commandLineArguments.isSet("all")) {
          for {
            printers <- createPrinters(taskDef, handler, loggers, commandLineArguments)
            reporter <- ClassRunner.createReporter(commandLineArguments, loader)
            ss       <- SpecStructure.linkedSpecifications(spec.structure(env), env, loader)
            sorted   <- safe(SpecStructure.topologicalSort(ss).getOrElse(ss))
            _        <- reporter.prepare(env, printers)(sorted.toList)
            stats    <- sorted.toList.map(Reporter.report(env, printers)).sequenceU
            _        <- Reporter.finalize(env, printers)(sorted.toList)
          } yield stats.foldMap(identity _)

        } else createPrinters(taskDef, handler, loggers, commandLineArguments).flatMap(printers => Reporter.report(env, printers)(spec.structure(env)))

      report.andFinally(Actions.safe(env.shutdown))
    }
  }

  /** accepted printers */
  def createPrinters(taskDef: TaskDef, handler: EventHandler, loggers: Array[Logger], args: Arguments): Action[List[Printer]] =
    List(createSbtPrinter(handler, loggers, sbtEvents(taskDef, handler)),
      createJUnitXmlPrinter(args, loader),
      createHtmlPrinter(args, loader),
      createMarkdownPrinter(args, loader),
      createPrinter(args, loader),
      createNotifierPrinter(args, loader)).map(_.map(_.toList)).sequenceU.map(_.flatten)

  def createSbtPrinter(h: EventHandler, ls: Array[Logger], e: SbtEvents) = {
    val arguments = Arguments(args: _*)

    if (!printerNames.map(_.name).exists(arguments.isSet) || arguments.isSet(CONSOLE.name))
      Actions.ok(Some {
        new SbtPrinter {
          lazy val handler = h
          lazy val loggers = ls
          lazy val events = e
        }
      })
    else noInstance("no console printer defined", arguments.verbose)
  }

  def sbtEvents(t: TaskDef, h: EventHandler) = new SbtEvents {
    lazy val taskDef = t
    lazy val handler = h
  }

  /**
   * Notify sbt of errors during the run
   */
  private def handleRunError(e: Throwable \/ String, loggers: Array[Logger], events: SbtEvents, arguments: Arguments) {
    val logger = SbtLineLogger(loggers)

    def logThrowable(t: Throwable) =
      Runner.logThrowable(t, arguments)(m => IO(logger.errorLine(m))).unsafePerformIO

    e.fold(
      t => { events.suiteError(t); logThrowable(t) },
      m => { events.suiteError; logger.errorLine(m) }
    )

    logger.close
  }

  /**
   * Notify sbt of warnings during the run
   */
  private def handleRunWarnings(warnings: Vector[String], loggers: Array[Logger], arguments: Arguments) {
    val logger = SbtLineLogger(loggers)
    Runner.logUserWarnings(warnings)(m => IO(logger.failureLine(m))).unsafePerformIO
    logger.close
  }
}


/**
 * Implementation of the Framework interface for the sbt tool.
 * It declares the classes which can be executed by the specs2 library.
 */
class Specs2Framework extends Framework {
  def name = "specs2"

  def fingerprints = Array[Fingerprint](fp1, fp1m)

  def runner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) =
    new SbtRunner(args, remoteArgs, loader)
}

object Fingerprints {
  val fp1 = new SpecificationFingerprint {
    override def toString = "specs2 Specification fingerprint"
  }
  val fp1m = new SpecificationFingerprint {
    override def toString = "specs2 Specification fingerprint"

    override def isModule = false
  }
}

trait SpecificationFingerprint extends SubclassFingerprint {
  def isModule = true

  def superclassName = "org.specs2.specification.core.SpecificationStructure"

  def requireNoArgConstructor = false
}


/**
 * This object can be used to debug the behavior of the SbtRunner
 */
object sbtRun extends SbtRunner(Array(), Array(), Thread.currentThread.getContextClassLoader) {
  def main(arguments: Array[String]) {
    exit(start(arguments: _*))
  }

  def exit(action: Action[Stats]) {
    runAction(action).fold(
      err => System.exit(100),
      ok  => if (ok.isSuccess) System.exit(0) else System.exit(1))
  }

  def start(arguments: String*): Action[Stats] = {
    if (arguments.isEmpty)
      ConsoleEffect.log("The first argument should at least be the specification class name") >>
      Actions.ok(Stats.empty)
    else {
      val taskDef = new TaskDef(arguments(0), Fingerprints.fp1, true, Array())
      specificationRun(taskDef, Thread.currentThread.getContextClassLoader, NoEventHandler, Array(ConsoleLogger), isModule = false)
    }
  }

}

object NoEventHandler extends EventHandler {
  def handle(event: Event) {}
}

object ConsoleLogger extends Logger {
  def ansiCodesSupported = false

  def error(message: String) = println("error: " + message)

  def info(message: String) = println("info: " + message)

  def warn(message: String) = println("warn: " + message)

  def debug(message: String) = println("debug: " + message)

  def trace(t: Throwable) = println("trace: " + t)
}
