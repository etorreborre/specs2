package org.specs2
package scalacheck

import org.scalacheck.util._

trait ScalaCheckParameters2 {
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters: Parameters = new Parameters()

  implicit def defaultFreqMapPretty: FreqMap[Set[Any]] => Pretty =
    Pretty.prettyFreqMap
}

