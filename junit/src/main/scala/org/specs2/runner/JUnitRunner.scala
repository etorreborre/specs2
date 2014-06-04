package org.specs2
package runner

import org.junit.runner.manipulation.{NoTestsRemainException, Filterable}
import org.junit.runner.notification.RunNotifier
import main._
import reporter._
import specification.core._
import control._

class JUnitRunner(klass: Class[_]) extends org.junit.runner.Runner with Filterable { outer =>

  /** specification to execute */
  lazy val specification =
    SpecificationStructure.create(klass.getName, Thread.currentThread.getContextClassLoader).execute(consoleLogging).unsafePerformIO.fold(
      ok => ok,
      error => error.fold(m => throw new Exception(m), t => throw t, (m, t) => throw t)
    )
  /** command line arguments, extracted from the system properties */
  lazy val arguments: Arguments = Arguments("junit")
  /** specification environment */
  lazy val env: Env = Env(arguments = arguments)

  /** specification structure for the environment */
  lazy val specStructure: SpecStructure = specification.structure(env)

  /** junit descriptions */
  lazy val descriptions = JUnitDescriptions.fragmentDescriptions(specStructure)

  /** @return a Description for the TestSuite */
  lazy val getDescription = JUnitDescriptions.createDescription(specStructure)

  /** run the specification with a Notifier */
  def run(n: RunNotifier) {
    val printer = new JUnitPrinter {
      lazy val notifier = n
      lazy val descriptions = outer.descriptions
      lazy val description = outer.getDescription
    }

    val actions =
      Reporter.report(env, Map(JUNIT -> printer, CONSOLE -> TextPrinter))(specStructure) >>
      Actions.safe(env.shutdown)

    actions.execute(consoleLogging).unsafePerformIO
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

