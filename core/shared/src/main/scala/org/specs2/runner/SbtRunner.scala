package org.specs2.runner

import org.specs2.specification.core.*
import org.specs2.specification.core.EnvDefault
import org.specs2.specification.process.*
import sbt.testing.{Runner as _, *}
import org.specs2.main.*
import org.specs2.reporter.*
import org.specs2.control.{Logger as _, *}
import org.specs2.fp.*
import org.specs2.fp.syntax.*
import org.specs2.reporter.SbtPrinterLogger
import org.specs2.reporter.Printer.*
import org.specs2.reflect.*
import org.specs2.fp.*, syntax.*
import org.specs2.concurrent.ExecutionEnv
import org.specs2.data.NamedTag
import scala.util.*
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, ExecutionContext}

/** Runner for Sbt
  */
abstract class BaseSbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader)
    extends _root_.sbt.testing.Runner:
  // loggers are populated when a task is executed
  // We need the loggers there to be able to report failures
  // when the environment is shutdown
  var loggers: Array[Logger] = Array()

  lazy val commandLineArguments = Arguments(args ++ remoteArgs*)

  lazy val env = EnvDefault.create(commandLineArguments).setCustomClassLoader(loader)

  def tasks(taskDefs: Array[TaskDef]): Array[Task] =
    taskDefs.toList.map(newTask).toArray

  /** create a new test task */
  def newTask(aTaskDef: TaskDef): Task =
    SbtTask(aTaskDef, env, loader, this)

  def done =
    env.shutdown
      .onComplete {
        case Failure(e) =>
          loggers.foreach(_.error("error while finalizing resources: " + e.getMessage))
        case Success(failures) =>
          failures.toList.foreach { result => loggers.foreach(_.error(result.message)) }
      }(using env.specs2ExecutionContext)
    ""

  def deserializeTask(task: String, deserializer: String => TaskDef): Task =
    newTask(deserializer(task))

  def serializeTask(task: Task, serializer: TaskDef => String): String =
    serializer(task.taskDef)

  def receiveMessage(msg: String): Option[String] =
    Some(msg)

  def isSlave: Boolean = false

case class MasterSbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader)
    extends BaseSbtRunner(args, remoteArgs, loader)

case class SlaveSbtRunner(args: Array[String], remoteArgs: Array[String], loader: ClassLoader, send: String => Unit)
    extends BaseSbtRunner(args, remoteArgs, loader):
  override def isSlave: Boolean = true

/** This object can be used to debug the behavior of the SbtRunner
  */
object sbtRun extends MasterSbtRunner(Array(), Array(), Thread.currentThread.getContextClassLoader):
  def main(arguments: Array[String]): Unit =
    val env = Env(Arguments(arguments*))
    given ee: ExecutionEnv = env.specs2ExecutionEnv

    try exit(start(arguments*))
    finally Action.future(env.shutdown).runVoid(ee)

  def exit(action: Action[Stats])(using ee: ExecutionEnv): Unit =
    action
      .runFuture(ee)
      .onComplete {
        case scala.util.Failure(_)     => System.exit(100)
        case scala.util.Success(stats) => if stats.isSuccess then System.exit(0) else System.exit(1)
      }(ee.executionContext)

  def start(arguments: String*): Action[Stats] = {
    val logger = ConsoleLogger()
    if arguments.isEmpty then
      logger.info("The first argument should at least be the specification class name").toAction >>
        Action.unit
    else
      val taskDef = new TaskDef(arguments(0), Fingerprints.fp1, true, Array())
      Action.pure(newTask(taskDef).execute(NoEventHandler, Array(ConsoleTestingLogger))).void
  }.as(Stats.empty)

object NoEventHandler extends EventHandler:
  def handle(event: Event): Unit = {}

object ConsoleTestingLogger extends Logger:
  def ansiCodesSupported = false

  def error(message: String) = println("error: " + message)

  def info(message: String) = println("info: " + message)

  def warn(message: String) = println("warn: " + message)

  def debug(message: String) = println("debug: " + message)

  def trace(t: Throwable) = println("trace: " + t)

case class SbtTask(aTaskDef: TaskDef, env: Env, loader: ClassLoader, base: BaseSbtRunner) extends sbt.testing.Task:

  private val arguments = env.arguments

  private given ec: ExecutionContext =
    env.specs2ExecutionContext

  /** @return the specification tags */
  def tags: Array[String] =
    lazy val spec = createSpecStructure(taskDef, loader, env).runOption.flatten
    lazy val tags: List[NamedTag] =
      spec.flatMap(s => s.tags.runOption(env.specs2ExecutionEnv)).getOrElse(Nil)

    if env.arguments.commandLine.isSet("sbt.tags") then tags.flatMap(_.names).toArray
    else Array()

  def execute(handler: EventHandler, loggers: Array[Logger], continuation: Array[Task] => Unit): Unit =
    executeFuture(handler, loggers).onComplete(_ => continuation(Array()))

  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] =
    Await.result(executeFuture(handler, loggers), Duration.Inf)
    Array()

  private def executeFuture(handler: EventHandler, loggers: Array[Logger]): Future[Unit] =
    val ee = env.specs2ExecutionEnv
    // pass the loggers back to the base runner for the final reporting
    base.loggers = loggers

    createSpecStructure(taskDef, loader, env).toAction.attempt
      .runFuture(ee)
      .flatMap {
        case Left(t) =>
          Future(processResult(handler, loggers)(t))

        case Right(None) =>
          Future(())

        case Right(Some(structure)) =>
          specificationRun(aTaskDef, structure, env, handler, loggers).attempt.runFuture(ee).map {
            case Left(t) => processResult(handler, loggers)(t)
            case _       => ()
          }
      }
      .recover { case t =>
        val events = sbtEvents(taskDef, handler)
        events.suiteError(t)
        loggers.foreach(_.trace(t))
      }

  /** @return the corresponding task definition */
  def taskDef = aTaskDef

  /** display errors and warnings */
  private def processResult[A](handler: EventHandler, loggers: Array[Logger])(t: Throwable): Unit =
    handleRunError(t, loggers, sbtEvents(taskDef, handler))

  /** run a given spec structure */
  private def specificationRun(
      taskDef: TaskDef,
      spec: SpecStructure,
      env: Env,
      handler: EventHandler,
      loggers: Array[Logger]
  ): Action[Stats] =

    val customInstances = CustomInstances(arguments, loader, env.systemLogger)
    val specFactory = DefaultSpecFactory(env, loader)
    val makeSpecs =
      if arguments.isSet("all") then
        specFactory
          .createLinkedSpecs(spec)
          .map(ss => SpecStructure.topologicalSort(ss)(env.specs2ExecutionEnv).getOrElse(ss))
      else Operation.pure(Seq(spec))

    for
      printers <- createPrinters(customInstances, taskDef, handler, loggers, arguments).toAction
      reporter <- Reporter
        .createCustomInstance(customInstances)
        .map(_.getOrElse(Reporter.create(printers, env)))
        .toAction
      allSpecs <- makeSpecs.toAction
      stats <- reporter.report(allSpecs*)
    yield stats

  /** create a spec structure from the task definition containing the class name */
  private def createSpecStructure(taskDef: TaskDef, loader: ClassLoader, env: Env): Operation[Option[SpecStructure]] =
    taskDef.fingerprint match
      case f: SubclassFingerprint =>
        if f.superclassName.endsWith("SpecificationStructure") then
          val className = taskDef.fullyQualifiedName + (if f.isModule then "$" else "")
          Classes
            .createInstance[SpecificationStructure](className, loader, EnvDefault.defaultInstances(env))
            .map(ss => Option(ss.structure))
        else Operation.ok(None)
      case _ => Operation.ok(None)

  /** accepted printers */
  private def createPrinters(
      customInstances: CustomInstances,
      taskDef: TaskDef,
      handler: EventHandler,
      loggers: Array[Logger],
      args: Arguments
  ): Operation[List[Printer]] =
    val printerFactory = PrinterFactory(arguments, customInstances, ConsoleLogger())

    List(
      createSbtPrinter(loggers, sbtEvents(taskDef, handler), customInstances),
      printerFactory.createJUnitXmlPrinter,
      printerFactory.createHtmlPrinter,
      printerFactory.createMarkdownPrinter,
      printerFactory.createPrinter,
      printerFactory.createNotifierPrinter
    ).map(_.map(_.toList)).sequence.map(_.flatten)

  /** create a SbtPrinter If printer names are passed on the command line we don't want to show the output of the spec
    * by default (unless `console` is specified as well to force the display of the spec). In that case we only dispatch
    * sbt events in order to report on the specification status
    */
  private def createSbtPrinter(
      loggers: Array[Logger],
      sbtEvents: SbtEvents,
      customInstances: CustomInstances
  ): Operation[Option[Printer]] =
    Operation.ok(Some {
      if printerNames.map(_.name).exists(arguments.isSet) && !arguments.isSet(CONSOLE.name) then
        SbtPrinter(env, loggers, sbtEvents, eventsOnly = true)
      else SbtPrinter(env, loggers, sbtEvents)
    })

  private def sbtEvents(t: TaskDef, h: EventHandler) = new SbtEvents {
    lazy val taskDef = t
    lazy val handler = h
  }

  /** Notify sbt of exceptions during the run
    */
  private def handleRunError(t: Throwable, loggers: Array[Logger], events: SbtEvents): Unit =
    val logger = SbtPrinterLogger(loggers)

    events.suiteError(t)
    logger.errorLine(t.getMessage)
    RunnerLogger(env).logThrowable(t).unsafeRun
    logger.close()
