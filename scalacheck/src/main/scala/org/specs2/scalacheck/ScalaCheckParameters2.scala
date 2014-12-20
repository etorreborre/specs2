package org.specs2
package scalacheck

trait ScalaCheckParameters2 {
  /**
   * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
   */
  implicit def defaultParameters: Parameters = new Parameters()
}

