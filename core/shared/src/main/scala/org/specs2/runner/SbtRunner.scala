package org.specs2
package runner

import Runner._
import specification.process.Stats
import sbt.testing._
import main._
import reporter._
import control.{Logger => _, _}
import org.specs2.fp._
import org.specs2.fp.syntax._
import reporter.SbtLineLogger
import reporter.Printer._
import specification.core._
import Actions._
import reflect._
import eff.ErrorEffect._
import org.specs2.concurrent.ExecutionEnv
import org.specs2.control.ExecuteActions._
import org.specs2.data.NamedTag

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Runner for Sbt
 */
abstract class BaseSbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader) extends _root_.sbt.testing.Runner {

  lazy val commandLineArguments = Arguments(args ++ remoteArgs: _*)

  lazy val env = Env(arguments = commandLineArguments, customClassLoader = Some(loader))

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.map(newTask).toArray

  /** create a new test task */
  def newTask(aTaskDef: TaskDef): Task =
    SbtTask(aTaskDef, env, loader)

  def done = {
    env.shutdown
    ""
  }

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    newTask(deserializer(task))

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef)

  def receiveMessage(msg: String): Option[String] =
    Some(msg)

  def isSlave: Boolean = false

}

case class MasterSbtRunner(args:       Array[String],
                           remoteArgs: Array[String],
                           loader:     ClassLoader) extends BaseSbtRunner(args, remoteArgs, loader)

case class SlaveSbtRunner(args:       Array[String],
                          remoteArgs: Array[String],
                          loader:     ClassLoader,
                          send:       String => Unit) extends BaseSbtRunner(args, remoteArgs, loader) {
  override def isSlave: Boolean = true
}

/**
 * This object can be used to debug the behavior of the SbtRunner
 */
object sbtRun extends MasterSbtRunner(Array(), Array(), Thread.currentThread.getContextClassLoader) {
  def main(arguments: Array[String]) {
    val env = Env(Arguments(arguments:_*))
    implicit def ee: ExecutionEnv = env.specs2ExecutionEnv
    try exit(start(arguments: _*))
    finally env.shutdown
  }

  def exit(action: Action[Stats])(implicit ee: ExecutionEnv): Unit = {
    runActionFuture(action)(ee).onComplete(_.fold(
      err => System.exit(100),
      ok  => if (ok.isSuccess) System.exit(0) else System.exit(1)))(ee.executionContext)
  }

  def start(arguments: String*): Action[Stats] = {
    if (arguments.isEmpty)
      log("The first argument should at least be the specification class name") >>
        Actions.unit
    else {
      val taskDef = new TaskDef(arguments(0), Fingerprints.fp1, true, Array())
      Actions.delayed(newTask(taskDef).execute(NoEventHandler, Array(ConsoleLogger))).as(())
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

case class SbtTask(aTaskDef: TaskDef, env: Env, loader: ClassLoader) extends sbt.testing.Task {

  private val arguments = env.arguments

  private implicit lazy val ec = env.specs2ExecutionContext

  /** @return the specification tags */
  def tags: Array[String] = {
    lazy val spec = runOperation(createSpecStructure(taskDef, loader, env)).toOption.flatten
    lazy val tags: List[NamedTag] =
      spec.flatMap(s => runAction(s.tags)(env.specs2ExecutionEnv).toOption).getOrElse(Nil)

    if (env.arguments.commandLine.isSet("sbt.tags"))
      tags.flatMap(_.names).toArray
    else
      Array()
  }

  def execute(handler: EventHandler, loggers: Array[Logger], continuation: Array[Task] => Unit): Unit =
    executeFuture(handler, loggers).onComplete(_ => continuation(Array()))

  private def executeFuture(handler: EventHandler, loggers: Array[Logger]): Future[Unit] = {
    val ee = env.specs2ExecutionEnv

    executeActionFuture(createSpecStructure(taskDef, loader, env))(ee).flatMap { case (result, warnings) =>
      processResult(handler, loggers)(result, warnings)
      result.toOption.flatten match {
        case Some(structure) =>
          executeActionFuture(specificationRun(aTaskDef, structure, env, handler, loggers))(ee).map { case (rs, ws) =>
            processResult(handler, loggers)(rs, ws)
          }

        case None =>
          Future(())
      }
    }.recover { case t => loggers.foreach(_.trace(t)) }

  }

  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    Await.result(executeFuture(handler, loggers), Duration.Inf)
    Array()
  }

  /** @return the correponding task definition */
  def taskDef = aTaskDef

  /** display errors and warnings */
  private def processResult[A](handler: EventHandler, loggers: Array[Logger])(result: Error Either A, warnings: List[String]): Unit =
    result match {
      case Left(e) =>
        if (warnings.nonEmpty) handleRunWarnings(warnings, loggers)
        else handleRunError(e, loggers, sbtEvents(taskDef, handler))

      case _ => handleRunWarnings(warnings, loggers)
    }

  /** run a given spec structure */
  private def specificationRun(taskDef: TaskDef, spec: SpecStructure, env: Env, handler: EventHandler, loggers: Array[Logger]): Action[Stats] =
    for {
      printers <- createPrinters(taskDef, handler, loggers, arguments).toAction
      stats    <- Runner.runSpecStructure(spec, env, loader, printers)
    } yield stats

  /** create a spec structure from the task definition containing the class name */
  private def createSpecStructure(taskDef: TaskDef, loader: ClassLoader, env: Env): Operation[Option[SpecStructure]] =
    taskDef.fingerprint match {
      case f: SubclassFingerprint =>
        if (f.superclassName.endsWith("SpecificationStructure")) {
          val className = taskDef.fullyQualifiedName + (if (f.isModule) "$" else "")
          Classes.createInstance[SpecificationStructure](className, loader, EnvDefault.defaultInstances(env)).
            map(ss => Option(ss.structure(env)))
        }
        else Operations.ok(None)
      case _ => Operations.ok(None)
    }


  /** accepted printers */
  private def createPrinters(taskDef: TaskDef, handler: EventHandler, loggers: Array[Logger], args: Arguments): Operation[List[Printer]] =
    List(
      createSbtPrinter(handler, loggers, sbtEvents(taskDef, handler)),
      createJUnitXmlPrinter(args, loader),
      createHtmlPrinter(args, loader),
      createMarkdownPrinter(args, loader),
      createPrinter(args, loader),
      createNotifierPrinter(args, loader)).map(_.map(_.toList)).sequence.map(_.flatten)

  private def createSbtPrinter(h: EventHandler, ls: Array[Logger], e: SbtEvents) = {
    if (!printerNames.map(_.name).exists(arguments.isSet) || arguments.isSet(CONSOLE.name))
      Operations.ok(Some {
        new SbtPrinter {
          lazy val handler = h
          lazy val loggers = ls
          lazy val events = e
        }
      })
    else noInstance("no console printer defined", arguments.verbose)
  }

  private def sbtEvents(t: TaskDef, h: EventHandler) = new SbtEvents {
    lazy val taskDef = t
    lazy val handler = h
  }

  /**
   * Notify sbt of errors during the run
   */
  private def handleRunError(e: Throwable Either String, loggers: Array[Logger], events: SbtEvents): Unit = {
    val logger = SbtLineLogger(loggers)

    def logThrowable(t: Throwable) =
      Runner.logThrowable(t, arguments)(m => Name(logger.errorLine(m))).value

    e match {
      case Left(t) =>
        events.suiteError(t)
        logger.errorLine(t.getMessage)
        logThrowable(t)

      case Right(m) =>
        events.suiteError
        logger.errorLine(m)
    }
    logger.close
  }

  /**
   * Notify sbt of warnings during the run
   */
  private def handleRunWarnings(warnings: List[String], loggers: Array[Logger]) {
    val logger = SbtLineLogger(loggers)
    Runner.logUserWarnings(warnings)(m => Name(logger.failureLine(m))).value
    logger.close
  }

}
