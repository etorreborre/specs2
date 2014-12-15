package org.specs2
package matcher

import org.scalacheck.{Test, Prop}
import org.scalacheck.util.Pretty
import org.specs2.io.Output

/**
 * This trait provides generation parameters to use with the `ScalaCheckMatchers`
 */
trait ScalaCheckParameters { outer: ScalaCheckMatchers with Output =>
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters = new Parameters()
  /** default parameters to display pretty messages */
  implicit def defaultPrettyParams = Pretty.defaultParams

  /** set specific execution parameters on a Property */
  implicit def setProperty(p: Prop) = new SetProperty(p)
  class SetProperty(prop: Prop) {
    /** create parameters with verbose = false */
    def set(minTestsOk: Int             = defaultParameters.minTestsOk,
            minSize: Int                = defaultParameters.minSize,
            maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
            maxSize: Int                = defaultParameters.maxSize,
            workers: Int                = defaultParameters.workers,
            rng: scala.util.Random      = defaultParameters.rng,
            callback: Test.TestCallback = defaultParameters.callback,
            loader: Option[ClassLoader] = defaultParameters.loader): execute.Result =
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, outer))
  }

  /** set specific execution parameters on a Property */
  implicit def displayProperty(p: Prop) = new DisplayProperty(p)
  class DisplayProperty(prop: Prop) {
    /** create parameters with verbose = true */
    def display(minTestsOk: Int             = defaultParameters.minTestsOk,
                minSize: Int                = defaultParameters.minSize,
                maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
                maxSize: Int                = defaultParameters.maxSize,
                workers: Int                = defaultParameters.workers,
                rng: scala.util.Random      = defaultParameters.rng,
                callback: Test.TestCallback = defaultParameters.callback,
                loader: Option[ClassLoader] = defaultParameters.loader): execute.Result =
      check(prop)(new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, outer))
  }

  /** create parameters with verbose = false */
  def set(minTestsOk: Int             = defaultParameters.minTestsOk,
          minSize: Int                = defaultParameters.minSize,
          maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
          maxSize: Int                = defaultParameters.maxSize,
          workers: Int                = defaultParameters.workers,
          rng: scala.util.Random      = defaultParameters.rng,
          callback: Test.TestCallback = defaultParameters.callback,
          loader: Option[ClassLoader] = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = false, outer)

  /** create parameters with verbose = true */
  def display(minTestsOk: Int             = defaultParameters.minTestsOk,
              minSize: Int                = defaultParameters.minSize,
              maxDiscardRatio: Float      = defaultParameters.maxDiscardRatio,
              maxSize: Int                = defaultParameters.maxSize,
              workers: Int                = defaultParameters.workers,
              rng: scala.util.Random      = defaultParameters.rng,
              callback: Test.TestCallback = defaultParameters.callback,
              loader: Option[ClassLoader] = defaultParameters.loader): Parameters =
    new Parameters(minTestsOk, minSize, maxDiscardRatio, maxSize, workers, rng, callback, loader, verbose = true, outer)

}
