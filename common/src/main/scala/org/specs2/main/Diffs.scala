package org.specs2
package main

import execute.{AsResult, BestMatching}
import control.Exceptions._
import text._
import NotNullStrings._
import EditDistance._

/**
 * this trait is used to define and compute the differences between values (used by the reporters)
 */
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean
  /** @return true if the differences must be shown for 2 different values */
  def show(expected: Any, actual: Any): Boolean
  /** @return true if the differences must be shown for 2 different sequences of values */
  def show(expected: Seq[Any], actual: Seq[Any], ordered: Boolean): Boolean
  /** @return the diffs */
  def showDiffs(expected: Any, actual: Any): (String, String)
  /** @return the diffs for sequences */
  def showDiffs(expected: Seq[Any], actual: Seq[Any], ordered: Boolean): (String, String)
  /** @return true if the full strings must also be shown */
  def showFull: Boolean
}

/**
 * The SmartDiffs class holds all the required parameters to show differences between 2 values
 * using the edit distance algorithm
 */
case class SmartDiffs(show: Boolean       = true,
                      separators: String  = "[]",  // Parameters for string diffs
                      triggerSize: Int    = 20,
                      shortenSize: Int    = 5,
                      diffRatio: Int      = 30,
                      showFull: Boolean   = false,
                      seqTriggerSize: Int = 0,    // Parameters for sequences
                      seqMaxSize: Int     = 100
                       ) extends Diffs {
  import EditDistance._

  def show(expected: Any, actual: Any): Boolean =
    show && Seq(expected, actual).exists(_.toString.size >= triggerSize)

  def show(expected: Seq[Any], actual: Seq[Any], ordered: Boolean): Boolean =
    show && (expected.size + actual.size) >= seqTriggerSize && (expected.size + actual.size) <= seqMaxSize

  def showDiffs(expectedValue: Any, actualValue: Any) = {
    val (expected, actual) = (expectedValue.toString, actualValue.toString)
    if (editDistance(expected, actual).doubleValue / (expected.size + actual.size) < diffRatio.doubleValue / 100)
      showDistance(expected, actual, separators, shortenSize)
    else
      (expected, actual)
  }

  /** @return the diffs for sequences */
  def showDiffs(expected: Seq[Any], actual: Seq[Any], ordered: Boolean): (String, String) = {
    val (added, missing) = addedAndMissing(expected, actual)
    if (added.isEmpty && missing.isEmpty) ("", "") else
      (if (missing.nonEmpty) "\n\nMissing values"+missing.map(display).mkString("\n", "\n", "\n") else "",
       if (added.nonEmpty) "\nAdditional values"+added.map(display).mkString("\n", "\n", "\n\n") else "")
  }

  /** @return added and missing values between 2 sequences */
  private def addedAndMissing(expected: Seq[Any], actual: Seq[Any]) = {
    val (matched, missingFromActual) = BestMatching.findBestMatch(actual, expected, (t: Any, v: Any) => v == t, eachCheck = true)(AsResult.booleanAsResult)
    val (_, koValues)                = matched.partition(_._3.isSuccess)
    val missingFromExpected          = koValues.map(_._1)
    val isEqual                      = missingFromActual.isEmpty && missingFromExpected.isEmpty

    (missingFromExpected.map(_.toString), missingFromActual.map(_.toString))
  }

  // display pairs nicely
  private val display = (a: Any) => a match {
    case (k, v) => s"$k -> $v"
    case _      => a.notNull
  }

}


object SmartDiffs {
  def fromString(s: String): Either[Exception, Diffs] = trye {
    val values = s.split(",")
    SmartDiffs(
      show           = boolean(values(0)),
      separators     = values(1),
      triggerSize    = values(2).toInt,
      shortenSize    = values(3).toInt,
      diffRatio      = values(4).toInt,
      showFull       = boolean(values(5)),
      seqTriggerSize = values(6).toInt,
      seqMaxSize     = values(6).toInt)
  }(identity)

  private def boolean(s: String) =
    if (Seq("true", "t").contains(s.trim.toLowerCase)) true
    else if (Seq("false", "f").contains(s.trim.toLowerCase)) false
    else throw new Exception(s+" is not a boolean value")

}

