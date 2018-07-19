package org.specs2
package runner

import org.junit.runner.notification._
import org.junit.runner.{Description, Result, RunWith}
import org.specs2.control.ExecuteActions._
import org.specs2.main.Arguments
import org.specs2.specification.BeforeAfterAll
import org.specs2.specification.core.{Env, OwnEnv}

import scala.collection.mutable.ListBuffer

class JUnitRunnerSpec(val env: Env) extends Specification with OwnEnv {
  def is =
    s2"""

      The Junit runner must run all linked specifications if 'all' is set on the command line $allSpecifications
      The Junit runner must run only examples $onlyExamples

      """

  def allSpecifications = {
    runSpecification(new JUnitRunner(classOf[MainJUnitSpecification])) { messages =>
      messages must contain("run started LinkedJUnitSpec", "run started MainJUnitSpecification")
    }
  }

  def onlyExamples = {
    runSpecification(new JUnitRunner(classOf[JUnitWithBeforeAfterAllSpecification])) { messages =>
      messages.toList === List(
        "run started JUnitWithBeforeAfterAllSpecification",
        "test started one example(org.specs2.runner.JUnitWithBeforeAfterAllSpecification)",
        "test finished one example(org.specs2.runner.JUnitWithBeforeAfterAllSpecification)",
        "run finished")
    }
  }

  private def runSpecification[T](runner: JUnitRunner)(assertMessages: ListBuffer[String] => T): T = {
    val (notifier, messages) = createNotifier()
    runner.runWithEnv(notifier, ownEnv.copy(arguments = Arguments("all", "junit"))).runOption(ee)
    assertMessages(messages)
  }

  private def createNotifier() = {
    val messages = new scala.collection.mutable.ListBuffer[String]
    val listener: RunListener = new RunListener {
      override def testRunStarted(description: Description): Unit =
        messages.append("run started " + description.getDisplayName)

      override def testRunFinished(result: Result): Unit =
        messages.append("run finished")

      override def testStarted(description: Description): Unit =
        messages.append("test started " + description.getDisplayName)

      override def testFinished(description: Description): Unit =
        messages.append("test finished " + description.getDisplayName)

      override def testFailure(failure: Failure): Unit = {
        messages.append("test failure " + failure.getDescription.getDisplayName + " " + failure.getMessage)
      }

      override def testIgnored(description: Description): Unit = {
        messages.append("test ignored " + description.getDisplayName)
      }

    }
    val notifier = new RunNotifier
    notifier.addListener(listener)
    (notifier, messages)
  }

}

@RunWith(classOf[JUnitRunner])
class MainJUnitSpecification extends Specification {
  def is =
    s2"""

  one example $ok
  and a ${"linked spec" ~ LinkedJUnitSpec}

"""
}

@RunWith(classOf[JUnitRunner])
object LinkedJUnitSpec extends mutable.Specification {

  "ok" >> ok

}

@RunWith(classOf[JUnitRunner])
class JUnitWithBeforeAfterAllSpecification extends Specification with BeforeAfterAll {
  def is =
    s2"""

  one example $ok

"""

  override def beforeAll(): Unit = {}

  override def afterAll(): Unit = {}
}
