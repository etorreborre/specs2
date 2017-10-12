package org.specs2
package guide

import control._
import io._
import Functions._
import org.specs2.fp.syntax._

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

    (lastBefore3 ++ officialsAfter3).sorted.reverse
  }

  def isGreaterThanEqualVersion3: VersionTag => Boolean = (tag: VersionTag) =>
    Ordering[DotNumber].gt(tag.number, DotNumber(List(3)))

  def isTimestamped: VersionTag => Boolean = (tag: VersionTag) =>
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
           "SPECS2-3.0.1-20150307223251-cdafed1a").flatMap(VersionTag.fromString)) ====
      List("SPECS2-2.4.17",
           "SPECS2-3.0",
           "SPECS2-3.0.1").flatMap(VersionTag.fromString)
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

  implicit def VersionTagOrdering: Ordering[VersionTag] = new Ordering[VersionTag] {
    def compare(x: VersionTag, y: VersionTag): Int =
      Ordering[(DotNumber, Option[String])].compare((x.number, x.timestamp), (y.number, y.timestamp))
  }
}

case class DotNumber(values: List[Int]) {
  def render: String =
    values.mkString(".")
}


import Exceptions._

object DotNumber {
  def fromString(s: String): Option[DotNumber] =
    tryo(DotNumber(s.split("\\.").toList.map(_.toInt)))

  implicit val DotNumberOrdering: Ordering[DotNumber] = new Ordering[DotNumber] {
    def compare(x: DotNumber, y: DotNumber): Int =
      if (x.values.zip(y.values).forall { case (n1, n2) => n1 > n2 }) 1
      else if (x.values == y.values) 0
      else -1
  }
}
