package org.specs2
package runner

import org.junit.runner.manipulation.{NoTestsRemainException, Filterable}
import org.junit.runner.notification.RunNotifier
import main._
import specification.process.Stats
import control.Actions._
import reporter._
import specification.core._
import control._
import org.specs2.fp.syntax._
import org.specs2.concurrent.ExecutionEnv
import ExecuteActions._

/**
 * Runner for specs2 specifications
 */
class JUnitRunner(klass: Class[_]) extends org.junit.runner.Runner with Filterable { outer =>

  /** specification to execute */
  lazy val specification = {
    val structure = SpecificationStructure.create(klass.getName, Thread.currentThread.getContextClassLoader, Some(env))
    runAction(structure, consoleLogging)(env.specs2ExecutionEnv).fold(
      error => error.fold(t => throw t, m => throw new Exception(m)),
      ok => ok
    )
  }

  /** command line arguments, extracted from the system properties */
  lazy val arguments: Arguments =
    Arguments("junit")

  /** specification environment */
  lazy val env: Env =
    Env(arguments = arguments, lineLogger = LineLogger.consoleLogger)

  lazy val getDescription =
    JUnitDescriptions.createDescription(specStructure)(env.specs2ExecutionEnv)

  /** specification structure for the environment */
  lazy val specStructure: SpecStructure =
    specification.structure(env)

  /** run the specification with a Notifier */
  def run(n: RunNotifier): Unit = {
    runWithEnv(n, env).runOption(env.specs2ExecutionEnv)
    ()
  }

  /** run the specification with a Notifier and an environment */
  def runWithEnv(n: RunNotifier, env: Env): Action[Stats] = {
    val loader = Thread.currentThread.getContextClassLoader
    val arguments = env.arguments

    val report: Action[Stats] =
      if (arguments.isSet("all")) {
        for {
          reporter <- ClassRunner.createReporter(arguments, loader).toAction
          printers <- ClassRunner.createPrinters(arguments, loader).toAction
          ss       <- SpecStructure.linkedSpecifications(specStructure, env, loader).toAction
          sorted   <- delayed(SpecStructure.topologicalSort(ss)(env.specs2ExecutionEnv).getOrElse(ss))
          _        <- reporter.prepare(env, printers)(sorted.toList)
          stats    <- sorted.toList.map(s => Reporter.report(env, createJUnitPrinter(s, n, env.specs2ExecutionEnv) +: printers)(s)).sequence
          _        <- Reporter.finalize(env, printers)(sorted.toList)
        } yield stats.foldMap(identity _)
      } else
        for {
          printers <- ClassRunner.createPrinters(arguments, loader).toAction
          stats    <- Reporter.report(env, createJUnitPrinter(specStructure, n, env.specs2ExecutionEnv) +: printers)(specStructure)
        } yield stats

    report
  }

  /**
   * create a printer for a specific specification structure
   * The printer needs to know about all the example descriptions beforehand
   */
  def createJUnitPrinter(specStructure: SpecStructure, n: RunNotifier, ee: ExecutionEnv): JUnitPrinter = new JUnitPrinter {
    lazy val notifier = n
    lazy val descriptionsTree = JUnitDescriptionsTree(specStructure, ee)
    lazy val descriptions = descriptionsTree.descriptions
    lazy val description = descriptionsTree.description
  }

  /**
   * This is used to filter out the entire specification based
   * on categories annotations
   *
   * if the more fine-grained filtering is needed tags must be used
   */
  def filter(filter: org.junit.runner.manipulation.Filter) {
    if (!filter.shouldRun(getDescription)) throw new NoTestsRemainException
  }
}
