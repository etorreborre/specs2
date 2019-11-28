package org.specs2
package runner

import control._
import io.StringOutput
import org.specs2.specification.process.Stats
import specification.core._
import reporter._
import main.Arguments
import org.specs2.fp.syntax._
import Runner._
import reporter.LineLogger._

/**
 * The class runner expects the first command-line argument to be the class name of
 * a specification to execute
 */
trait ClassRunner {

  /**
   * run a specification but don't exit with System.exit
   */
  def run(args: Array[String]): Unit = {
    run(args, exit = false)
  }

  /**
   * run the specification, the first argument is expected to be the specification name
   */
  def run(args: Array[String], exit: Boolean): Unit = {
    val arguments = Arguments(args.drop(1): _*)
    val env = Env(arguments = arguments, lineLogger = consoleLogger)

    val actions: Action[Stats] = args.toList match {
      case Nil =>
        Action.fail("there must be at least one argument, the fully qualified class name") >>
        Action.pure(Stats.empty)

      case className :: rest =>
        for {
          spec  <- createSpecification(className, Thread.currentThread.getContextClassLoader, Some(env)).toAction
          stats <- report(env)(spec)
        } yield stats
    }
    try execute(actions, arguments, exit)(env)
    finally env.shutdown
  }

  /** create the specification from the class name */
  def createSpecification(className: String, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader, env: Option[Env] = None): Operation[SpecificationStructure] =
    SpecificationStructure.create(className, classLoader, env)

  /** report the specification */
  def report(env: Env, logger: Logger = ConsoleLogger()): SpecificationStructure => Action[Stats] = { spec: SpecificationStructure =>
    val loader = Thread.currentThread.getContextClassLoader
    for {
      printers <- createPrinters(env.arguments, loader, logger).toAction
      stats    <- Runner.runSpecStructure(spec.structure(env), env, loader, printers)
    } yield stats
  }

  /** accepted printers */
  def createPrinters(args: Arguments, loader: ClassLoader, logger: Logger = ConsoleLogger()): Operation[List[Printer]] =
    List(createTextPrinter(args, loader, logger),
      createJUnitXmlPrinter(args, loader, logger),
      createHtmlPrinter(args, loader, logger),
      createMarkdownPrinter(args, loader, logger),
      createPrinter(args, loader, logger),
      createNotifierPrinter(args, loader, logger)).sequence.map(_.flatten)

  /** custom or default reporter */
  def createReporter(args: Arguments, loader: ClassLoader, logger: Logger = ConsoleLogger()): Operation[Reporter] =
    createCustomInstance[Reporter](args, loader, "reporter",
      (m: String) => "a custom reporter can not be instantiated " + m, "no custom reporter defined, using the default one", logger)
      .map(_.getOrElse(Reporter))

}

object ClassRunner extends ClassRunner

object consoleRunner extends ClassRunner {
  def main(args: Array[String]) =
    run(args, exit = true)
}

/**
 * Test runner to simulate a console run
 */
object TextRunner extends ClassRunner {
  def run(spec: SpecificationStructure, args: Arguments = Arguments())(env: Env): LineLogger with StringOutput = {
    val logger = LineLogger.stringLogger
    val env1 = env.setLineLogger(logger).setArguments(env.arguments.overrideWith(args))
    report(env1)(spec).runAction(env1.specs2ExecutionEnv)
    logger
  }

  override def createPrinters(args: Arguments, loader: ClassLoader, logger: Logger = ConsoleLogger()): Operation[List[Printer]] =
    List(createTextPrinter(args, loader, logger)).sequence.map(_.flatten)
}
