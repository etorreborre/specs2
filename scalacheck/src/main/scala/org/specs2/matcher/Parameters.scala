package org.specs2
package matcher

import org.scalacheck.Test
import org.scalacheck.util.Pretty
import org.scalacheck.util.Pretty._
import org.specs2.io.{ConsoleOutput, Output}

/**
 * This class is the base class for the display and set case classes.<br>
 * It contains a Map of generation parameters and indicates if the generation
 * must be verbose.
 */
case class Parameters(minTestsOk: Int             = Test.Parameters.default.minSuccessfulTests,
                      minSize: Int                = Test.Parameters.default.minSize,
                      maxDiscardRatio: Float      = Test.Parameters.default.maxDiscardRatio,
                      maxSize: Int                = Test.Parameters.default.maxSize,
                      workers: Int                = Test.Parameters.default.workers,
                      rng: scala.util.Random      = Test.Parameters.default.rng,
                      callback: Test.TestCallback = Test.Parameters.default.testCallback,
                      loader: Option[ClassLoader] = Test.Parameters.default.customClassLoader,
                      verbose: Boolean            = false,
                      output: Output              = ConsoleOutput) { outer =>

  def testCallback = if (verbose) verboseCallback.chain(callback) else callback

  def toScalaCheckParameters: Test.Parameters =
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

  def verboseCallback = new Test.TestCallback {
    override def onPropEval(name: String, threadXdx: Int, succeeded: Int, discarded: Int): Unit = {
      if (discarded == 0) output.printf("\rPassed %d tests", succeeded)
      else                output.printf("\rPassed %d tests; %d discarded", succeeded, discarded)
    }
    override def onTestResult(name: String, result: Test.Result) = {
      val s = prettyTestRes(result)(Pretty.defaultParams)
      output.printf("\r%s %s%s\n", if (result.passed) "+" else "!", s, List.fill(70 - s.length)(" ").mkString(""))
    }
  }

}

