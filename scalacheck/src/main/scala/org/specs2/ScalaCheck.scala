package org.specs2

import matcher.{MatchResult, Parameters, MatchersImplicits}
import specification._
import core._
import create._
import org.scalacheck.Prop
import MatchersImplicits._

/**
 * The ScalaCheck trait can be used to access ScalaCheck matchers
 */
trait ScalaCheck extends matcher.ScalaCheckMatchers with AutoExamples { this: FragmentsFactory =>
}
