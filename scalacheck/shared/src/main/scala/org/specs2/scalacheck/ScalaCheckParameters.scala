package org.specs2
package scalacheck

import org.scalacheck.util._
import org.scalacheck.Test
import PrettyDetails._
import org.scalacheck.rng.Seed

trait ScalaCheckParameters {
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters: Parameters = Parameters()
  implicit def defaultSeed: Seed = Seed.random

  implicit def defaultFreqMapPretty: FreqMap[Set[Any]] => Pretty = (fq: FreqMap[Set[Any]]) =>
    Pretty.prettyFreqMap(removeDetails(fq))

  /** create parameters with verbose = false */
  def set(minTestsOk: Int                              = defaultParameters.minTestsOk,
          minSize: Int                                 = defaultParameters.minSize,
          maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
          maxSize: Int                                 = defaultParameters.maxSize,
          workers: Int                                 = defaultParameters.workers,
          callback: Test.TestCallback                  = defaultParameters.testCallback,
          loader: Option[ClassLoader]                  = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, callback, loader)

  /** create parameters with verbose = true */
  def display(minTestsOk: Int                              = defaultParameters.minTestsOk,
              minSize: Int                                 = defaultParameters.minSize,
              maxDiscardRatio: Float                       = defaultParameters.maxDiscardRatio,
              maxSize: Int                                 = defaultParameters.maxSize,
              workers: Int                                 = defaultParameters.workers,
              callback: Test.TestCallback                  = defaultParameters.testCallback,
              loader: Option[ClassLoader]                  = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, callback, loader).verbose
}

