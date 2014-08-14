package org.specs2
package reporter

import control._
import main.Arguments
import matcher._
import execute._
import LineLogger._
import data.Fold
import specification.create.S2StringContext
import specification.dsl.FragmentsDsl
import specification.core._
import specification.process._
import ControlMatchers._
import scalaz.concurrent.Task

class ReporterSpec extends Specification with ForEachEnv with ThrownExpectations { def is = s2"""

 The reporter is responsible for:
   - filtering the specification fragments
   - executing the specification
   - running various printers
   - saving the specification state

 A specification is
   filtered                       $a1
   executed                       $a2

 And at the end of the reporting
   the spec stats are saved       $a3
   the example stats are saved    $a4

 Different printers can be used with the reporter
   by default the text printer is used       $b1
   other printers (then printer is not used) $b2
   unless console is in the arguments        $b3

"""

  import reporterSpecSupport._

  def a1 = { env: Env =>
    val logger = stringLogger
    reported(env.setArguments(Arguments("ex ex3")).setLineLogger(logger))
    logger.messages.mkString("\n") must contain("ex3")
    logger.messages.mkString("\n") must not contain("ex1")
  }

  def a2 = { env: Env =>
    val logger = stringLogger
    reported(env.setLineLogger(logger), logger)
    indexOf(logger.messages, "e3") must be_<(indexOf(logger.messages, "e1"))
  }

  def a3 = { env: Env =>
    val repository = StatisticsRepository.memory
    reported(env.setStatisticRepository(repository))
    repository.getStatistics(spec().specClassName) must beOk(beSome((_: Stats).examples must_== 3))
  }

  def a4 = { env: Env =>
    val repository = StatisticsRepository.memory
    reported(env.setStatisticRepository(repository))
    val ex2 = spec().fragments.fragments(3)
    repository.previousResult(spec().specClassName, ex2.description) must beOk(beSome((_: Result).isFailure must beTrue))
  }

  def b1 = { env: Env =>
    val logger = stringLogger
    reported(env.setLineLogger(logger), logger)
    logger.messages must not(beEmpty)
  }

  def b2 = { env: Env =>
    val logger = stringLogger
    reported(env.setLineLogger(logger).setArguments(Arguments("junit")), printers = List(new FakeJUnitPrinter(logger)))
    logger.messages must not(contain("ex1"))
    logger.messages must contain("[info] junit")
  }

  def b3 = { env: Env =>
    val logger = stringLogger

    reported(env.setLineLogger(logger).setArguments(Arguments("console junit")),
      printers = List(TextPrinter, new FakeJUnitPrinter(logger)))

    val messages = logger.messages
    messages must contain("ex1")
    messages must contain("[info] junit")
  }

}

class FakeJUnitPrinter(logger: LineLogger) extends Printer {
  def fold(env: Env, spec: SpecStructure) =
    Fold.fromFunction((f: Fragment) => Task.now(logger.infoLog("junit\n")))
}

object reporterSpecSupport extends MustMatchers with StandardMatchResults with S2StringContext with FragmentsDsl {
  /**
   * TEST METHODS
   */
  lazy val reporter = new Reporter {}

  def spec(logger: LineLogger = NoLineLogger): SpecStructure = s2"""
 ex1 ${ex1(logger)}
 ex2 ${ex2(logger)}
 ex3 ${ex3(logger)}
 """

  def ex1(logger: LineLogger) = { Thread.sleep(100); logger.infoLog("e1\n"); ok}
  def ex2(logger: LineLogger) = { logger.infoLog("e2\n"); ko }
  def ex3(logger: LineLogger) = { logger.infoLog("e3\n"); ok }

  def reported(env: Env, logger: LineLogger = NoLineLogger, printers: List[Printer] = List(TextPrinter)) =
    reporter.report(env, printers)(spec(logger)).runOption

  def indexOf(messages: Seq[String])(f: String => Boolean): Int =
    messages.zipWithIndex.find { case (s, i) => f(s)}.fold(-1)(_._2)

  def indexOf(messages: Seq[String], element: String): Int =
    indexOf(messages)((_: String).contains(element))

}
