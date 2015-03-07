package org.specs2
package guide

import control._
import io._
import Functions._
import scalaz._, Scalaz._

/**
 * Functions for finding the most relevant specs2 tags to display on the website
 */
trait Specs2Tags {
  def allTags: Action[List[VersionTag]] =
    Executable.execute(FilePath("git"), Seq("tag")).map(_.trim.split("\n").map(VersionTag.fromString).toList.flatten)

  def publishedTags: Action[List[VersionTag]] =
    allTags.map(filterPublished)

  def filterPublished(tags: List[VersionTag]): List[VersionTag] = {
    val lastBefore3     = tags.filter(!isGreaterThanEqualVersion3).sorted.lastOption.toList
    val officialsAfter3 = tags.filter(isGreaterThanEqualVersion3 && !isTimestamped)
    val latest          = latestTag(tags).toList

    (lastBefore3 ++ officialsAfter3 ++ latest).sorted
  }

  def isGreaterThanEqualVersion3 = (tag: VersionTag) =>
    tag.number >= DotNumber(List(3))

  def isTimestamped = (tag: VersionTag) =>
    tag.timestamp.isDefined

  /** find the latest timestamped tag */
  def latestTag(tags: List[VersionTag]): Option[VersionTag] =
    tags.filter(isTimestamped).sorted.lastOption

}

object Specs2Tags extends Specs2Tags

import Specs2Tags._

class Specs2TagsSpec extends Specification { def is = s2"""

 ${ VersionTag.fromString("SPECS2-2.4.17-20150307203418-cdafed1a").exists(isTimestamped) }
 ${ VersionTag.fromString("SPECS2-2.4.17").exists(!isTimestamped) }
 ${ filterPublished(
      List("SPECS2-2.4.9",
           "SPECS2-2.4.16",
           "SPECS2-2.4.17",
           "SPECS2-3.0",
           "SPECS2-3.0-20150307203418-cdafed1a",
           "SPECS2-3.0.1",
           "SPECS2-3.0.1-20150307203418-cdafed1a",
           "SPECS2-3.0.1-20150307223251-cdafed1a").map(VersionTag.fromString).flatten) ====
      List("SPECS2-2.4.17",
           "SPECS2-3.0",
           "SPECS2-3.0.1",
           "SPECS2-3.0.1-20150307223251-cdafed1a").map(VersionTag.fromString).flatten
  }
"""
}

case class VersionTag(number: DotNumber, timestamp: Option[String], commit: Option[String]) {
  def render =
    (List("SPECS2", number.render) ++ timestamp.toList ++ commit.toList).mkString("-")
}

object VersionTag {
  def fromString(s: String): Option[VersionTag] = {
    s.split("\\-").toList match {
      case _ :: number :: timestamp :: commit :: Nil =>
        DotNumber.fromString(number).map(dotNumber => VersionTag(dotNumber, Some(timestamp), Some(commit)))

      case _ :: number :: Nil =>
        DotNumber.fromString(number).map(dotNumber => VersionTag(dotNumber, None, None))

      case _ => None
    }
  }

  implicit def VersionTagOrder: Order[VersionTag] =
    Order.orderBy[VersionTag, (DotNumber, Option[String])](vt => (vt.number, vt.timestamp))

  implicit def VersionTagOrdering: scala.Ordering[VersionTag] =
    VersionTagOrder.toScalaOrdering
}

case class DotNumber(values: List[Int]) {
  def render: String =
    values.mkString(".")
}


import Exceptions._

object DotNumber {
  def fromString(s: String): Option[DotNumber] =
    tryo(DotNumber(s.split("\\.").toList.map(_.toInt)))

  implicit def DotNumberOrder: Order[DotNumber] =
    Order.orderBy[DotNumber, List[Int]](_.values)
}
