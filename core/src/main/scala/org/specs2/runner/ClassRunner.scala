package org.specs2
package runner

import control._
import Actions._
import io.StringOutput
import specification.core._
import reporter._
import main.Arguments
import reporter.Printer._
import scalaz._, Scalaz._
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
  def run(args: Array[String]) {
    run(args, exit = false)
  }

  /**
   * run the specification, the first argument is expected to be the specification name
   */
  def run(args: Array[String], exit: Boolean) {
    val arguments = Arguments(args.drop(1):_*)
    val env = Env(arguments = arguments, lineLogger = consoleLogger)

    val actions: Action[Unit] = args.toList match {
      case Nil =>
        Actions.fail("there must be at least one argument, the fully qualified class name")

      case className :: rest =>
        createSpecification(className, Thread.currentThread.getContextClassLoader, Some(env)).flatMap[Unit](report(env)) >>
        Actions.safe(env.shutdown)
    }
    execute(actions, arguments, exit)
  }

  /** create the specification from the class name */
  def createSpecification(className: String, classLoader: ClassLoader = Thread.currentThread.getContextClassLoader, env: Option[Env] = None): Action[SpecificationStructure] =
    SpecificationStructure.create(className, classLoader, env)

  /** report the specification */
  def report(env: Env): SpecificationStructure => Action[Unit] = { spec: SpecificationStructure =>
    val loader = Thread.currentThread.getContextClassLoader
    if (env.arguments.isSet("all")) {
      for {
        printers <- createPrinters(env.arguments, loader)
        reporter <- createReporter(env.arguments, loader)
        ss       <- SpecStructure.linkedSpecifications(spec.structure(env), env, loader)
        sorted   <- safe(SpecStructure.topologicalSort(ss).getOrElse(ss))
        _        <- reporter.prepare(env, printers)(sorted.toList)
        _        =  sorted.toList.map(Reporter.report(env, printers)).sequenceU.void
        _        <- Reporter.finalize(env, printers)(sorted.toList)
      } yield ()

    } else
      createPrinters(env.arguments, loader).map(printers => Reporter.report(env, printers)(spec.structure(env))).void
  }

  /** accepted printers */
  def createPrinters(args: Arguments, loader: ClassLoader): Action[List[Printer]] =
    List(createTextPrinter(args, loader),
         createJUnitXmlPrinter(args, loader),
         createHtmlPrinter(args, loader),
         createMarkdownPrinter(args, loader),
         createPrinter(args, loader),
         createNotifierPrinter(args, loader)).sequenceU.map(_.flatten)

  /** custom or default reporter */
  def createReporter(args: Arguments, loader: ClassLoader): Action[Reporter] =
    createCustomInstance[Reporter](args, loader, "reporter", (m: String) => "a custom reporter can not be instantiated "+m, "no custom reporter defined, using the default one")
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
  def run(spec: SpecificationStructure, args: Arguments = Arguments())(implicit env: Env = Env()): LineLogger with StringOutput = {
    val logger = LineLogger.stringLogger
    try {
      val env1 = env.setLineLogger(logger).setArguments(env.arguments.overrideWith(args))
      report(env1)(spec).execute(env.systemLogger).unsafePerformIO
      logger
    } finally env.shutdown
  }

  override def createPrinters(args: Arguments, loader: ClassLoader): Action[List[Printer]] =
    List(createTextPrinter(args, loader)).sequenceU.map(_.flatten)
}

