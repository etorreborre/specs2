package org.specs2
package runner

import org.junit.runner.manipulation.{Filterable, NoTestsRemainException}
import org.junit.runner.notification.{Failure, RunNotifier}
import main._
import fp.syntax._
import control._
import specification.core._
import specification.process._
import reporter._
import scala.util.control.NonFatal

/**
 * Runner for specs2 specifications
 */
class JUnitRunner(klass: Class[_]) extends org.junit.runner.Runner with Filterable { outer =>

  /** specification to execute */
  lazy val specification = {
    val structure = SpecificationStructure.create(klass.getName, Thread.currentThread.getContextClassLoader, Some(env))
    structure.unsafeRun
  }

  /** command line arguments, extracted from the system properties */
  lazy val arguments: Arguments =
    Arguments("junit")

  /** specification environment */
  lazy val env: Env =
    EnvDefault.create(arguments)

  lazy val getDescription: org.junit.runner.Description =
    getDescription(env)

  def getDescription(env: Env): org.junit.runner.Description =
    try JUnitDescriptions.createDescription(specStructure)(env.specs2ExecutionEnv)
    catch { case NonFatal(t) => env.shutdown; throw t; }

  /** specification structure for the environment */
  lazy val specStructure: SpecStructure =
    specification.structure(env)

  /** run the specification with a Notifier */
  def run(n: RunNotifier): Unit = {
    try {
      runWithEnv(n, env).runAction(env.specs2ExecutionEnv) match {
        case Right(_) => ()
        case Left(t) => n.fireTestFailure(new Failure(getDescription, new RuntimeException(t)))
      }
    }
    finally env.shutdown
  }

  /** run the specification with a Notifier and an environment */
  def runWithEnv(runNotifier: RunNotifier, env: Env): Action[Stats] = {
    val loader = Thread.currentThread.getContextClassLoader
    val arguments = env.arguments
    val customInstances = CustomInstances(arguments, loader, env.systemLogger)
    val printerFactory = PrinterFactory(arguments, customInstances, env.systemLogger)
    val junitPrinter = JUnitPrinter(env, runNotifier)

    for {
      printers <- printerFactory.createPrinters.toAction
      reporter <- Reporter.createCustomInstance(customInstances).map(_.getOrElse(Reporter.create(junitPrinter +: printers, env))).toAction
      stats    <- reporter.report(specStructure)
     } yield stats
  }

  /**
   * This is used to filter out the entire specification based
   * on categories annotations
   *
   * if the more fine-grained filtering is needed tags must be used
   */
  def filter(filter: org.junit.runner.manipulation.Filter): Unit = {
    if (!filter.shouldRun(getDescription)) throw new NoTestsRemainException
  }
}
