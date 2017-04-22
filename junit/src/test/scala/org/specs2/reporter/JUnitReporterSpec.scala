package org.specs2
package reporter

import _root_.org.junit.ComparisonFailure
import _root_.org.junit.runner.Description
import _root_.org.junit.runner.notification.{Failure, RunNotifier}
import io._
import main.Arguments
import mock.Mockito
import matcher.MustThrownMatchers
import matcher.ActionMatchers._
import specification.core._

import scala.collection.mutable.ListBuffer

class JUnitReporterSpec extends Specification with Mockito with ForEachEnv {  def is = s2"""

 The JUnitRunner is meant to be used with the RunWith annotation.
 It takes a Specification, executes it and notifies a RunNotifier object of the possible
 failures.

 The following examples show the result of running a Specification with different
 successes or failures:


 If the specification has
   1 ok example, there must be a testStarted/testFinished notification                                ${notified().e1}
   2 ok examples, there must be a testStarted/testFinished notification for each                      ${notified().e2}
   1 failing example, there must be a testStarted/testFailure notification                            ${notified().e3}
   1 failing example, the failure message must be reported                                            ${notified().e4}
   1 example with an error, the error message must be reported                                        ${notified().e5}
   1 skipped example, a test ignored must be reported                                                 ${notified().e6}
   1 pending example, a test ignored must be reported                                                 ${notified().e7}
   1 failing example with be_==, a ComparisonFailure message must be reported                         ${notified().e8}
   steps must be correctly sequenced with examples                                                    ${notified().e9}
                                                                                                      """

  case class notified() extends WithNotifier with ReporterExamples {
    def desc(s: String) = =~(s) ^^ ((_:Description).getDisplayName)

    def e1 = { env: Env =>
      run(ex1)(env)
      Seq(there was one(notifier).fireTestStarted(desc("ex1")),
        there was one(notifier).fireTestFinished(desc("ex1")))
    }
    def e2 = { env: Env =>
      run(level1)(env)
      Seq("ex1", "ex2") flatMap { s =>
        Seq(there was one(notifier).fireTestStarted(desc(s)),
            there was one(notifier).fireTestFinished(desc(s)))
      }
    }
    def e3 = { env: Env =>
      run(ex1Failure)(env)
      Seq(there was one(notifier).fireTestStarted(desc("ex1")),
        there was one(notifier).fireTestFailure(any[Failure]))
    }
    def e4 = { env: Env =>
      run(ex1Failure)(env)
      val c = capture[Failure]
      there was one(notifier).fireTestFailure(c)
      c.value.getMessage must_== "failure"
    }
    def e5 = { env: Env =>
      run(ex1Error)(env)
      there was one(notifier).fireTestFailure(be_==("error")^^((_:Failure).getMessage))
    }
    def e6 = { env: Env =>
      run(ex1Skipped)(env)
      there was one(notifier).fireTestIgnored(desc("ex1"))
    }
    def e7 = { env: Env =>
      run(ex1Pending)(env)
      there was one(notifier).fireTestIgnored(desc("ex1"))
    }
    def e8 = { env: Env =>
      run(ex1BeEqualToFailure)(env)
      val c = capture[Failure]
      there was one(notifier).fireTestFailure(c)
      c.value.getException must haveSuperclass[ComparisonFailure]
    }

    def e9 = { env: Env =>
      val messages = new ListBuffer[String]
      run {
        step {
          messages += "before"
        } ^
        "ex1" ! { messages += "ex1"; ok } ^
        step {
          messages += "after"
        }
      }(env)
      messages.toList === Seq("before", "ex1", "after")
    }
  }

  /**
   * TEST METHODS
   */
  trait WithNotifier extends MustThrownMatchers { outer =>
    lazy val notifier = mock[RunNotifier]

    lazy val messagesNotifier = new RunNotifier with StringOutput {
      override def fireTestRunStarted(desc: Description) { println("run started "+desc) }
      override def fireTestRunFinished(result: org.junit.runner.Result) { println("run finished "+result) }
      override def fireTestStarted(desc: Description) { println("test started "+desc) }
      override def fireTestFailure(failure: org.junit.runner.notification.Failure) { println("test failed "+failure) }
      override def fireTestIgnored(desc: Description) { println("test ignored "+desc) }
      override def fireTestFinished(desc: Description) { println("test finished "+desc) }
    }

    def run(f: Fragment)(env: Env): Unit = run(Fragments(f))(env)
    def run(fs: Fragments)(env: Env): Unit = run(SpecStructure.create(SpecHeader(getClass), Arguments(), fs))(env)
    
    def run(spec: SpecStructure)(env: Env): Unit = {
      val reporter = Reporter
      val junitPrinter: Printer = new JUnitPrinter {
        def notifier = outer.notifier
        def descriptions = JUnitDescriptions.fragmentDescriptions(spec)(env.specs2ExecutionEnv)
        def description = JUnitDescriptions.specDescription(spec)
      }
      reporter.report(env, List(junitPrinter))(spec) must beOk
      ()
    }
  }

}
