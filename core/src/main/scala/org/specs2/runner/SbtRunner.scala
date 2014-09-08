package org.specs2
package runner

import sbt.testing._
import Fingerprints._
import main._
import reporter._
import control.{Logger => _, _}
import Actions._
import control.Throwablex
import scalaz.\&/
import scalaz.effect.IO
import scalaz.std.anyVal._
import scalaz.syntax.traverse._
import scalaz.std.list._
import reporter.SbtLineLogger
import reflect.Classes
import reporter.Printer._
import specification.core._

/**
 * Runner for Sbt
 */
case class SbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) extends _root_.sbt.testing.Runner {
  private lazy val commandLineArguments = Arguments(args:_*)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.map(newTask).toArray

  def newTask = (aTaskDef: TaskDef) =>
    new Task {
      def tags = Array[String]()
      def execute(handler: EventHandler, loggers: Array[Logger]) = {
        taskDef.fingerprint match {
          case f: SubclassFingerprint    =>
            if (f.superclassName.endsWith("SpecificationStructure")) {
              val action = specificationRun(aTaskDef, loader, handler, loggers, isModule = f.isModule)
              action.execute(consoleLogging).unsafePerformIO.fold(
                ok => ok,
                e  => handleRunError(e, loggers, sbtEvents(taskDef, handler), commandLineArguments)
              )
            }
            else ()
          case _  => ()
        }
        // nothing more to execute
        Array[Task]()
      }
      def taskDef = aTaskDef
    }

  def done = ""

  def specificationRun(taskDef: TaskDef, loader: ClassLoader, handler: EventHandler, loggers: Array[Logger], isModule: Boolean): Action[Unit] = {
    Classes.createInstance[SpecificationStructure](taskDef.fullyQualifiedName+(if (isModule) "$" else ""), loader).flatMap { spec =>
      val env = Env(arguments = commandLineArguments)

      val report: Action[Unit] =
      if (commandLineArguments.commandLine.contains("all")) {
        for {
          printers <- createPrinters(taskDef, handler, loggers, commandLineArguments)
          ss       <- SpecificationStructure.linkedSpecifications(spec, env, loader)
          sorted   <- safe(SpecificationStructure.topologicalSort(env)(ss).getOrElse(ss) :+ spec)
          rs = sorted.toList.map(s => Reporter.report(env, printers)(s.structure(env))).sequenceU
        } yield ()
        
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
    val arguments = Arguments(args.mkString(" "))

    if (!printerNames.map(_.name).exists(args.contains) || arguments.commandLine.isDefined(CONSOLE.name))
      Actions.ok(Some {
        new SbtPrinter {
          lazy val handler = h
          lazy val loggers = ls
          lazy val events = e
      }})
    else noPrinter("no console printer defined", arguments.verbose)
  }


  def sbtEvents(t: TaskDef, h: EventHandler) = new SbtEvents {
    lazy val taskDef = t
    lazy val handler = h
  }

  /**
   * Notify sbt of errors during the run
   */
  private def handleRunError(e: String \&/ Throwable, loggers: Array[Logger], events: SbtEvents, arguments: Arguments) {
    val logger = SbtLineLogger(loggers)

    def logThrowable(t: Throwable) =
      Runner.logThrowable(t, arguments)(m => IO(logger.errorLine(m))).unsafePerformIO

    e.fold(
      m =>      { events.error; logger.errorLine(m) },
      t =>      { events.error(t); if (t.getMessage != null) logger.errorLine(t.getMessage); logThrowable(t) },
      (m, t) => { events.error(t); if (t.getMessage != null) logger.errorLine(t.getMessage); logThrowable(t) }
    )
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
  val fp1  =  new SpecificationFingerprint { override def toString = "specs2 Specification fingerprint" }
  val fp1m =  new SpecificationFingerprint { override def toString = "specs2 Specification fingerprint"; override def isModule = false }
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
    exit(start(arguments:_*))
  }

  def exit(action: Action[Unit]) {
     action.execute(consoleLogging).unsafePerformIO.fold(
       ok  => System.exit(0),
       err => System.exit(1))
  }

  def start(arguments: String*): Action[Unit] = {
    if (arguments.length == 0)
      control.log("The first argument should at least be the specification class name")
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
  def info(message: String)  = println("info: " + message)
  def warn(message: String)  = println("warn: " + message)
  def debug(message: String) = println("debug: " + message)
  def trace(t: Throwable)    = println("trace: " + t)
}
