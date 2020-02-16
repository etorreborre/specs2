package org.specs2
package reporter

import _root_.org.junit.runner.Description
import _root_.org.junit.runner.notification.{RunNotifier}
import io._
import main.Arguments
import matcher.MustThrownMatchers
import matcher.ActionMatchers._
import specification.core._
import scala.collection.mutable.ListBuffer

class JUnitReporterSpec(val env: Env) extends Specification with OwnEnv {  def is = s2"""

 The JUnitRunner is meant to be used with the RunWith annotation.
 It takes a Specification, executes it and notifies a RunNotifier object of the possible
 failures.

 The following examples show the result of running a Specification with different
 successes or failures:


 If the specification has
   1 ok example, there must be a testStarted/testFinished notification           ${notified().e1}
   2 ok examples, there must be a testStarted/testFinished notification for each ${notified().e2}
   1 failing example, there must be a testStarted/testFailure notification       ${notified().e3}
   1 failing example, the failure message must be reported                       ${notified().e4}
   1 example with an error, the error message must be reported                   ${notified().e5}
   1 skipped example, a test ignored must be reported                            ${notified().e6}
   1 pending example, a test ignored must be reported                            ${notified().e7}
   1 failing example with be_==, a ComparisonFailure message must be reported    ${notified().e8}
   steps must be correctly sequenced with examples                               ${notified().e9}
"""

  case class notified() extends WithNotifier with ReporterExamples {
    def desc(s: String) = =~(s) ^^ ((_:Description).getDisplayName)

    def e1 = {
      run(ex1)(Env())
      notifier.messages must contain(
        "fireTestStarted ex1(notified)", "fireTestFinished ex1(notified)")
    }
    def e2 = {
      run(level1)(Env())
      notifier.messages must contain(
        "fireTestStarted level1::ex1(notified)", "fireTestFinished level1::ex1(notified)",
        "fireTestStarted level1::ex2(notified)", "fireTestFinished level1::ex2(notified)")
    }
    def e3 = {
      run(ex1Failure)(Env())
      notifier.messages must contain(
        "fireTestStarted ex1(notified)", "fireTestFailure ex1(notified): failure")
    }
    def e4 = {
      run(ex1Failure)(Env())
      notifier.messages must contain("fireTestFailure ex1(notified): failure")
    }
    def e5 = {
      run(ex1Error)(Env())
      notifier.messages must contain("fireTestFailure ex1(notified): error")
    }
    def e6 = {
      run(ex1Skipped)(Env())
      notifier.messages must contain("fireTestIgnored ex1(notified)")
    }
    def e7 = {
      run(ex1Pending)(Env())
      notifier.messages must contain("fireTestIgnored ex1(notified)")
    }
    def e8 = {
      run(ex1BeEqualToFailure)(Env())
      notifier.messages must contain(
        "fireTestStarted ex1(notified)", "fireTestFailure ex1(notified): 1 != 2 expected:<[2]> but was:<[1]>")
    }
    def e9 = {
      val messages = new ListBuffer[String]
      run {
        step {
          messages += "before"
        } ^
        "ex1" ! { messages += "ex1"; ok } ^
        step {
          messages += "after"
        }
      }(Env())
      messages.toList === Seq("before", "ex1", "after")
    }
  }

  /**
   * TEST METHODS
   */
  trait WithNotifier extends MustThrownMatchers { outer =>

    lazy val notifier = new RunNotifier with StringOutput {
      override def fireTestRunStarted(desc: Description): Unit = { println("fireTestRunStarted "+desc) }
      override def fireTestRunFinished(result: org.junit.runner.Result): Unit = { println("fireTestRunFinished "+result) }
      override def fireTestStarted(desc: Description): Unit = { println("fireTestStarted "+desc) }
      override def fireTestFailure(failure: org.junit.runner.notification.Failure): Unit = { println("fireTestFailure "+failure) }
      override def fireTestIgnored(desc: Description): Unit = { println("fireTestIgnored "+desc) }
      override def fireTestFinished(desc: Description): Unit = { println("fireTestFinished "+desc) }
    }

    def run(f: Fragment)(env: Env): Unit = run(Fragments(f))(env)
    def run(fs: Fragments)(env: Env): Unit = run(SpecStructure.create(SpecHeader(getClass), Arguments(), fs))(env)

    def run(spec: SpecStructure)(env: Env): Unit = {
      val junitPrinter: Printer = JUnitPrinter(env, outer.notifier)
      val reporter = Reporter.create(List(junitPrinter), env)
      reporter.report(spec) must beOk
      ()
    }
  }

}
