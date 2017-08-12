package org.specs2
package runner

import org.junit.runner.{Description, Result, RunWith}
import org.junit.runner.notification._
import main.Arguments
import specification.core.{Env, OwnEnv}
import control._
import ExecuteActions._

class JUnitRunnerSpec(val env: Env) extends Specification with OwnEnv { def is = s2"""

 The Junit runner must run all linked specifications if 'all' is set on the command line $allSpecifications

"""

  def allSpecifications = {
    val runner = createRunner(new MainJUnitSpecification)
    runner.runWithEnv(runner.notifier, ownEnv.copy(arguments = Arguments("all", "junit"))).runOption(ee)
    runner.messages must contain("run started LinkedJUnitSpec", "run started MainJUnitSpecification")
  }

  def createRunner(s: Specification) = {
    new JUnitRunner(classOf[MainJUnitSpecification]) {
      val messages = new scala.collection.mutable.ListBuffer[String]
      val listener = new RunListener {
        override def testRunStarted (description: Description): Unit =
          messages.append("run started "+description.getDisplayName)

        override def testRunFinished(result: Result): Unit =
          messages.append("run finished")

        override def testStarted (description: Description): Unit =
          messages.append("test started "+description.getDisplayName)

        override def testFinished(description: Description): Unit =
          messages.append("test finished "+description.getDisplayName)
      }
      val notifier = new RunNotifier
      notifier.addListener(listener)
    }
  }

}

@RunWith(classOf[JUnitRunner])
class MainJUnitSpecification extends Specification { def is = s2"""

  one example $ok
  and a ${ "linked spec" ~ LinkedJUnitSpec }

"""
}

@RunWith(classOf[JUnitRunner])
object LinkedJUnitSpec extends mutable.Specification {

  "ok" >> ok

}
