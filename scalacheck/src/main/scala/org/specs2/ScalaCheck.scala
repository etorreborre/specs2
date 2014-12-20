package org.specs2

import org.specs2.scalacheck._
import specification._
import create._


trait ScalaCheck extends
  ScalaCheckPropertyCreation with
  ScalaCheckPropertyCheck with
  ScalaCheckParameters2 with
  AsResultProp with
  ScalaCheckPropertyDsl with
  GenInstances {

}

/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheckOld extends matcher.ScalaCheckMatchers { this: FragmentsFactory =>
}
