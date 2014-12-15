package org.specs2

import specification._
import create._

/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers { this: FragmentsFactory =>
}
