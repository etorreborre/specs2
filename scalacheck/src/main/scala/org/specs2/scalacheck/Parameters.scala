package org.specs2
package scalacheck

import org.scalacheck.Test
import org.scalacheck.util.Pretty

/**
 * This class encapsulates ScalaCheck parameters + any additional parameters
 */
case class Parameters(minTestsOk: Int                 = Test.Parameters.default.minSuccessfulTests,
                      minSize: Int                    = Test.Parameters.default.minSize,
                      maxDiscardRatio: Float          = Test.Parameters.default.maxDiscardRatio,
                      maxSize: Int                    = Test.Parameters.default.maxSize,
                      workers: Int                    = Test.Parameters.default.workers,
                      rng: scala.util.Random          = Test.Parameters.default.rng,
                      testCallback: Test.TestCallback = Test.Parameters.default.testCallback,
                      loader: Option[ClassLoader]     = Test.Parameters.default.customClassLoader,
                      prettyParams: Pretty.Params     = Pretty.defaultParams) { outer =>

  def verbose: Parameters =
    setVerbosity(1)

  def setVerbosity(v: Int): Parameters =
    copy(prettyParams = prettyParams.copy(verbosity = v))

  def testParameters: Test.Parameters =
    new Test.Parameters {
      val minSuccessfulTests = outer.minTestsOk
      val maxDiscardRatio    = outer.maxDiscardRatio
      val maxSize            = outer.maxSize
      val minSize            = outer.minSize
      val workers            = outer.workers
      val rng                = outer.rng
      val testCallback       = outer.testCallback
      val customClassLoader  = outer.loader
    }
}
