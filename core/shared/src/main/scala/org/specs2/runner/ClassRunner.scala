package org.specs2
package runner

import control.*
import io.StringOutput
import specification.process.*
import specification.core.*
import reporter.*
import main.Arguments
import fp.syntax.*
import Runner.*
import scala.concurrent.*

trait ClassRunner:
  def run(className: String): Action[Stats]
  def run(spec: SpecificationStructure): Action[Stats]
  def run(spec: SpecStructure): Action[Stats]

/** A runner for Specification classes based on their names
  */

case class DefaultClassRunner(env: Env, reporter: Reporter, specFactory: SpecFactory) extends ClassRunner:

  val arguments: Arguments =
    env.arguments

  /** instantiate a Specification from its class name and use arguments to determine how to execute it and report
    * results
    */
  def run(className: String): Action[Stats] =
    specFactory.createSpecification(className).toAction.flatMap(spec => run(spec.structure)) |||
      Action
        .pure(println("cannot instantiate the specification: " + className + ". Please check your classpath"))
        .as(Stats.empty)

  def run(spec: SpecificationStructure): Action[Stats] =
    run(spec.structure)

  def run(specStructure: SpecStructure): Action[Stats] =
    val allSpecs = arguments.isSet("all")
    if allSpecs then
      for
        ss <- specFactory.createLinkedSpecs(specStructure).toAction
        sorted <- Action.pure(SpecStructure.topologicalSort(ss)(env.specs2ExecutionEnv).getOrElse(ss))
        stats <- reporter.report(sorted.toList)
      yield stats
    else reporter.report(specStructure)

trait ClassRunnerMain:
  /** run a specification but don't exit with System.exit
    */
  def run(args: Array[String]): Unit =
    run(args, exit = false)

  /** run the specification, the first argument is expected to be the specification name The class runner expects the
    * first command-line argument to be the class name of a specification to execute
    */
  def run(args: Array[String], exit: Boolean): Unit =
    val arguments = Arguments(args.drop(1)*)
    val env = EnvDefault.create(arguments)

    val actions: Action[Stats] = args.toList match
      case List() =>
        Action.fail("there must be at least one argument, the fully qualified class name") >>
          Action.pure(Stats.empty)

      case className :: rest =>
        for
          classRunner <- createClassRunner(env).toAction
          stats <- classRunner.run(className)
        yield stats

    try execute(actions, env, exit)
    finally env.awaitShutdown()

  /** Create a ClassRunner from the default environment containing the command line arguments
    */
  def createClassRunner(env: Env): Operation[ClassRunner] =
    val arguments = env.arguments
    val loader = env.getClass.getClassLoader
    val customInstances = CustomInstances(arguments, loader, env.systemLogger)
    val printerFactory = PrinterFactory(arguments, customInstances, env.systemLogger)
    val specFactory = DefaultSpecFactory(env, loader)

    for
      printers <- printerFactory.createPrinters
      reporter <- Reporter.createCustomInstance(customInstances).map(_.getOrElse(Reporter.create(printers, env)))
    yield DefaultClassRunner(env, reporter, specFactory)

object ClassRunner extends ClassRunnerMain

object consoleRunner extends ClassRunnerMain:
  def main(args: Array[String]) =
    run(args, exit = true)

/** Test runner to simulate a console run
  */
object TextRunner extends ClassRunnerMain:

  def run(spec: SpecificationStructure, arguments: Arguments = Arguments())(env: Env): PrinterLogger & StringOutput =
    val logger = PrinterLogger.stringPrinterLogger
    val env1 = env.setPrinterLogger(logger).setArguments(env.arguments.overrideWith(arguments))
    val loader = Thread.currentThread.getContextClassLoader
    val customInstances = CustomInstances(arguments, loader, StringOutputLogger(logger))

    val action =
      for
        reporter <- customInstances
          .createCustomInstance[Reporter](
            "reporter",
            (m: String) => "a custom reporter can not be instantiated " + m,
            "no custom reporter defined, using the default one"
          )
          .map(_.getOrElse(Reporter.create(List(TextPrinter(env1)), env1)))
          .toAction
        stats <- reporter.report(spec.structure)
      yield stats

    action.runAction(env1.specs2ExecutionEnv)
    logger

  /** this method returns a Future and does not try to instantiate any class so it is suitable for ScalaJS */
  def runFuture(spec: SpecificationStructure, arguments: Arguments = Arguments())(
      env: Env
  ): Future[PrinterLogger & StringOutput] =
    val logger = PrinterLogger.stringPrinterLogger
    val env1 = env.setPrinterLogger(logger).setArguments(env.arguments.overrideWith(arguments))
    given ExecutionContext = env1.executionContext
    val reporter = Reporter.create(List(TextPrinter(env1)), env1)
    reporter.report(spec.structure).runFuture(env1.specs2ExecutionEnv).map(_ => logger)
