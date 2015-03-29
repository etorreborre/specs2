package org.specs2
package main

import control.Exceptions._
import text._
import execute.{AsResult, BestMatching}
import control.Exceptions._
import text._
import NotNullStrings._
import StringEditDistance._

/**
 * Define and compute the differences between strings (used by the printers)
 */
trait Diffs {
  /** @return true if the differences must be shown */
  def show: Boolean
  /** @return true if the differences must be shown for 2 different values */
  def show(actual: Any, expected: Any): Boolean
  /** @return true if the differences must be shown for 2 different sequences of values */
  def showSeq(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): Boolean
  /** @return true if the differences must be shown for 2 different maps */
  def showMap(actual: Map[Any, Any], expected: Map[Any, Any]): Boolean
  /** @return the diffs */
  def showDiffs(actual: Any, expected: Any): (String, String)
  /** @return the diffs for sequences with missing / added values  */
  def showSeqDiffs(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): (Seq[String], Seq[String])
  /** @return the diffs for sequences with missing / added values  */
  def showMapDiffs(actual: Map[Any, Any], expected: Map[Any, Any]): (Seq[String], Seq[String], Seq[String])
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
                      seqMaxSize: Int     = 1000000
                       ) extends Diffs {
  import StringEditDistance._

  def show(actual: Any, expected: Any): Boolean =
    show && Seq(actual, expected).exists(_.notNull.size >= triggerSize)

  def showSeq(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): Boolean =
    show && (expected.size + actual.size) >= seqTriggerSize && (expected.size + actual.size) <= seqMaxSize

  def showMap(actual: Map[Any, Any], expected: Map[Any, Any]): Boolean =
    showSeq(actual.toSeq, expected.toSeq, ordered = false)

  def showDiffs(actualValue: Any, expectedValue: Any) = {
    val (actual, expected) = (actualValue.notNull, expectedValue.notNull)
    if (editDistance(actual, expected).doubleValue / (actual.size + expected.size) < diffRatio.doubleValue / 100)
      showDistance(actual, expected, separators, shortenSize)
    else
      (actual, expected)
  }

  /** @return the diffs for sequences */
  def showSeqDiffs(actual: Seq[Any], expected: Seq[Any], ordered: Boolean): (Seq[String], Seq[String]) = {
    val (matched, missing) = BestMatching.findBestMatch(actual, expected, (t: Any, v: Any) => v == t, eachCheck = true)(AsResult.booleanAsResult)
    val (_, koValues)      = matched.partition(_._3.isSuccess)
    val added              = koValues.map(_._1)

    (added.map(_.notNull), missing.map(_.notNull))
  }

  /** @return the diffs for maps */
  def showMapDiffs(actual: Map[Any, Any], expected: Map[Any, Any]): (Seq[String], Seq[String], Seq[String]) = {
    val (added, missing) = (actual.keySet.diff(expected.keySet), expected.keySet.diff(actual.keySet))
    val different        = actual.toSeq.collect { case (k,v) if expected.contains(k) && v != expected(k) =>
      s"  x key = $k\n    actual value\n    $v\n    expected value\n    ${expected(k)}"
    }

    (added.map(k => (k, actual(k))).map(notNullPair).toSeq, missing.map(k => (k, expected(k))).map(notNullPair).toSeq, different.map(notNullPair))
  }

}


object SmartDiffs {
  def fromString(s: String): Either[Throwable, Diffs] = trye {
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

