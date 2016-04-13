package org.specs2
package runner

import Runner._
import specification.process.Stats
import sbt.testing._
import Fingerprints._
import main._
import reporter._
import control.{Logger => _, _}

import scalaz._
import Scalaz._
import scalaz.effect.IO
import reporter.SbtLineLogger
import reflect.Classes
import reporter.Printer._
import specification.core._
import eff._
import ErrorEffect._
import Eff._

/**
 * Runner for Sbt
 */
case class SbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) extends _root_.sbt.testing.Runner {
  private lazy val commandLineArguments = Arguments(args: _*)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.map(newTask).toArray

  /** create a new test task */
  def newTask(aTaskDef: TaskDef) =
    new Task {
      lazy val env = Env(arguments = commandLineArguments)

      // the specification to execute with error messages if it cannot be instantiated
      lazy val specStructure: (Error \/ Option[SpecStructure], Vector[String]) = {
        val action: Action[Option[SpecStructure]] =
          createSpecStructure(taskDef, loader, env)
        executeAction(action)
      }

      /** @return the specification tags */
      def tags: Array[String] =
        specStructure._1.toOption.flatten.map(_.tags.flatMap(_.names).toArray).getOrElse(Array())

      def execute(handler: EventHandler, loggers: Array[Logger]) = {
        val (result, warnings) = specStructure
        processResult(handler, loggers)(result, warnings)

        result.toOption.flatten.foreach { structure =>
          val action = specificationRun(aTaskDef, structure, env, handler, loggers)
          val (rs, ws) = executeAction(action, consoleLogging)
          processResult(handler, loggers)(rs, ws)
        }
        // nothing more to execute
        Array[Task]()
      }

      /** @return the correponding task definition */
      def taskDef = aTaskDef

      /** display errorrs and warnings */
      def processResult[A](handler: EventHandler, loggers: Array[Logger])(result: Error \/ A, warnings: Vector[String]): Unit = {
        result.fold(
          e => {
            if (warnings.nonEmpty) handleRunWarnings(warnings, loggers, commandLineArguments)
            else handleRunError(e, loggers, sbtEvents(taskDef, handler), commandLineArguments)
          },
          _ => handleRunWarnings(warnings, loggers, commandLineArguments)
        )
      }
    }

  def done = ""

  /** run a given spec structure */
  def specificationRun(taskDef: TaskDef, spec: SpecStructure, env: Env, handler: EventHandler, loggers: Array[Logger]): Action[Stats] =
    for {
      printers <- createPrinters(taskDef, handler, loggers, commandLineArguments)
      stats    <- Runner.runSpecStructure(spec, env, loader, printers)
    } yield stats

  /** create a spec structure from the task definition containing the class name */
  def createSpecStructure(taskDef: TaskDef, loader: ClassLoader, env: Env): Action[Option[SpecStructure]] =
    taskDef.fingerprint match {
      case f: SubclassFingerprint =>
        if (f.superclassName.endsWith("SpecificationStructure")) {
          val className = taskDef.fullyQualifiedName + (if (f.isModule) "$" else "")
          Classes.createInstance[SpecificationStructure](className, loader, env.defaultInstances).
            map(ss => Option(ss.structure(env)))
        }
        else Actions.ok(None)
      case _ => Actions.ok(None)
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
      Actions.unit
    else {
      val taskDef = new TaskDef(arguments(0), Fingerprints.fp1, true, Array())
      Actions.safe(newTask(taskDef).execute(NoEventHandler, Array(ConsoleLogger))).as(())
    }
  }.as(Stats.empty)

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
