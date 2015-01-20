package org.specs2
package scalacheck

import org.scalacheck.util._
import org.scalacheck.Test

trait ScalaCheckParameters {
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters: Parameters = new Parameters()

  implicit def defaultFreqMapPretty: FreqMap[Set[Any]] => Pretty =
    Pretty.prettyFreqMap

  /** create parameters with verbose = false */
  def set(minTestsOk: Int                              = defaultParameters.minTestsOk,
          minSize: Int                                 = defaultParameters.minSize,
          maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
          maxSize: Int                                 = defaultParameters.maxSize,
          workers: Int                                 = defaultParameters.workers,
          rng: scala.util.Random                       = defaultParameters.rng,
          callback: Test.TestCallback                  = defaultParameters.testCallback,
          loader: Option[ClassLoader]                  = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader)

  /** create parameters with verbose = true */
  def display(minTestsOk: Int                              = defaultParameters.minTestsOk,
              minSize: Int                                 = defaultParameters.minSize,
              maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
              maxSize: Int                                 = defaultParameters.maxSize,
              workers: Int                                 = defaultParameters.workers,
              rng: scala.util.Random                       = defaultParameters.rng,
              callback: Test.TestCallback                  = defaultParameters.testCallback,
              loader: Option[ClassLoader]                  = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader).verbose
}

