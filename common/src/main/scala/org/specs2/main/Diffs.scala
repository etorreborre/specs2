package org.specs2
package main

import control.Exceptions._
import text._

/**
 * this trait is used to define and compute the differences between strings (used by the reporters)
 */
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean
  /** @return true if the differences must be shown for 2 different strings */
  def show(expected: String, actual: String): Boolean
  /** @return the diffs */
  def showDiffs(expected: String, actual: String): (String, String)
  /** @return true if the full strings must also be shown */
  def showFull: Boolean
}

/**
 * The SmartDiffs class holds all the required parameters to show differences between 2 strings using the edit distance
 * algorithm
 */
case class SmartDiffs(show: Boolean = true, separators: String = "[]", triggerSize: Int = 20, shortenSize: Int = 5, diffRatio: Int = 30, showFull: Boolean = false) extends Diffs {
  import EditDistance._

  def show(expected: String, actual: String): Boolean = show && Seq(expected, actual).exists(_.size >= triggerSize)
  def showDiffs(expected: String, actual: String) = {
    if (editDistance(expected, actual).doubleValue / (expected.size + actual.size) < diffRatio.doubleValue / 100)
      showDistance(expected, actual, separators, shortenSize)
    else
      (expected, actual)
  }
}

object SmartDiffs {
  def fromString(s: String): Either[Exception, Diffs] = trye {
    val values = s.split(",")
    SmartDiffs(show = boolean(values(0)),
               separators = values(1),
               triggerSize = values(2).toInt,
               shortenSize = values(3).toInt,
               diffRatio = values(4).toInt,
               showFull = boolean(values(5)))
  }(identity)

  private def boolean(s: String) =
    if (Seq("true", "t").contains(s.trim.toLowerCase)) true
    else if (Seq("false", "f").contains(s.trim.toLowerCase)) false
    else throw new Exception(s+" is not a boolean value")

}
