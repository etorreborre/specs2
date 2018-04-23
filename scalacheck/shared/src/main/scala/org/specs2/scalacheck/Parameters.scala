package org.specs2
package scalacheck

import org.scalacheck.Test
import org.scalacheck.util.Pretty
import main.CommandLine
import org.scalacheck.rng.Seed

/**
 * This class encapsulates ScalaCheck parameters + any additional parameters
 */
case class Parameters(minTestsOk: Int                 = Test.Parameters.default.minSuccessfulTests,
                      minSize: Int                    = Test.Parameters.default.minSize,
                      maxDiscardRatio: Float          = Test.Parameters.default.maxDiscardRatio,
                      maxSize: Int                    = Test.Parameters.default.maxSize,
                      workers: Int                    = Test.Parameters.default.workers,
                      testCallback: Test.TestCallback = Test.Parameters.default.testCallback,
                      loader: Option[ClassLoader]     = Test.Parameters.default.customClassLoader,
                      prettyParams: Pretty.Params     = Pretty.defaultParams,
                      seed: Seed                      = Seed.random) { outer =>

  def verbose: Parameters =
    setVerbosity(1)

  def setVerbosity(v: Int): Parameters =
    copy(prettyParams = prettyParams.copy(verbosity = v))

  def testParameters: Test.Parameters =
    Test.Parameters.default.
      withMinSuccessfulTests(outer.minTestsOk).
      withMaxDiscardRatio(outer.maxDiscardRatio).
      withMaxSize(outer.maxSize).
      withMinSize(outer.minSize).
      withWorkers(outer.workers).
      withTestCallback(outer.testCallback).
      withCustomClassLoader(outer.loader)

  def overrideWith(commandLine: CommandLine): Parameters = {
    val updated =
      copy(
        minTestsOk      = commandLine.intOr  ("scalacheck.mintestsok",      minTestsOk),
        minSize         = commandLine.intOr  ("scalacheck.minsize",         minSize),
        maxDiscardRatio = commandLine.floatOr("scalacheck.maxdiscardratio", maxDiscardRatio),
        maxSize         = commandLine.intOr  ("scalacheck.maxsize",         maxSize),
        workers         = commandLine.intOr  ("scalacheck.workers",         workers),
        seed            = Seed(commandLine.longOr ("scalacheck.seed",       seed.long._1))
      ).setVerbosity(     commandLine.intOr  ("scalacheck.verbosity",       prettyParams.verbosity))

    if (commandLine.boolOr("scalacheck.verbose", false)) updated.verbose
    else                                                 updated
  }
}
