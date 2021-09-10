package org.specs2
package guide

import control.*
import io.*
import Functions.*
import org.specs2.fp.syntax.*
import math.Ordering.Implicits.*

/** Functions for finding the most relevant specs2 tags to display on the website
  */
trait Specs2Tags:
  def allTags: Operation[List[VersionTag]] =
    Executable.execute(FilePath("git"), Seq("tag")).map(_.trim.split("\n").toList.map(VersionTag.fromString).flatten)

  def publishedTags: Operation[List[VersionTag]] =
    allTags.map(filterPublished)

  def filterPublished(tags: List[VersionTag]): List[VersionTag] =
    tags.filter(isGreaterThanVersion(4)).groupBy(_.major).map(_._2).map(_.sorted.last).toList.sorted

  def isGreaterThanVersion(n: Int): VersionTag => Boolean = (tag: VersionTag) =>
    Ordering[DotNumber].gteq(tag.number, DotNumber(List(n)))

object Specs2Tags extends Specs2Tags

import Specs2Tags.*

class Specs2TagsSpec extends Specification:
  def is = s2"""

 ${filterPublished(
    List("SPECS2-3.9.4",
         "SPECS2-4.10.0",
         "SPECS2-4.12.1",
         "SPECS2-5.0.0-RC-01",
         "SPECS2-5.0.0-RC-10").flatMap(VersionTag.fromString)) ===
    List("SPECS2-4.12.1",
         "SPECS2-5.0.0-RC-10").flatMap(VersionTag.fromString)}
"""


case class VersionTag(number: DotNumber, timestamp: Option[String], commit: Option[String]):
  def major: Int =
    number.major

  def render =
    (List("SPECS2", number.render) ++ timestamp.toList ++ commit.toList).mkString("-")

object VersionTag:

  def fromString(s: String): Option[VersionTag] =
    s.split("\\-").toList match {
      case _ :: number :: timestamp :: commit :: List() =>
        DotNumber.fromString(number).map(dotNumber => VersionTag(dotNumber, Some(timestamp), Some(commit)))

      case _ :: number :: List() =>
        DotNumber.fromString(number).map(dotNumber => VersionTag(dotNumber, None, None))

      case _ => None
    }

  given Ordering[VersionTag] with
    def compare(x: VersionTag, y: VersionTag): Int =
      Ordering[(DotNumber, Option[String])].compare((x.number, x.timestamp), (y.number, y.timestamp))

case class DotNumber(values: List[Int]):
  def major: Int =
    values.head

  def render: String =
    values.mkString(".")

import Exceptions.*

object DotNumber:
  def fromString(s: String): Option[DotNumber] =
    tryo(DotNumber(s.split("\\.").toList.map(_.toInt)))

  given Ordering[DotNumber] with
    def compare(x: DotNumber, y: DotNumber): Int =
      Ordering[List[Int]].compare(x.values, y.values)
