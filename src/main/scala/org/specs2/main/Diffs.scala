package org.specs2
package main

import text._
import EditDistance._

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
  /** @return the separators to use*/
  def separators: String
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
