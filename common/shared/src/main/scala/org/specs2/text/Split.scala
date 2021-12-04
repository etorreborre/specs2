package org.specs2
package text

import Trim.*
import collection.Seqx.*
import util.matching.Regex
import scala.annotation.*

private[specs2] trait Split:
  outer: Split =>

  extension (s: String)
    @targetName("stringSplitDashed")
    def splitDashed(names: Seq[String]): Seq[String] =
      outer.splitDashed(s.split("\\s").toIndexedSeq, names)

    @targetName("stringSplitQuoted")
    def splitQuoted: Seq[String] =
      quoted.findAllIn(s).toSeq.map(_.trimEnclosing("\""))

    @targetName("stringSplitToSize")
    def splitToSize(n: Int): List[String] =
      outer.splitToSize(s, n, Nil)

  private val quoted: Regex =
    "\"[^\"]*\"|[^\\s]+".r

  private def splitToSize(string: String, n: Int, result: List[String]): List[String] =
    if string.length <= n then (string :: result).reverse
    else
      // new Strings are necessary to avoid memory errors because substring is just a view on the underlying string
      splitToSize(new String(string.drop(n)), n, new String(string.take(n)) :: result)

  extension (seq: Seq[String])
    @targetName("seqSplitDashed")
    def splitDashed(names: Seq[String]): Seq[String] =
      outer.splitDashed(seq, names)

  /** split a string along some names which start with a dash:
    *
    * "-include hello world -with me".splitDashed(Seq("include", "with")) === ("include", "hello world", "with", "me")
    */
  def splitDashed(seq: Seq[String], names: Seq[String]): Seq[String] =
    val dashedNames = names.map("-" + _.toLowerCase)
    def isDashedName(name: String) = dashedNames.contains(name.toLowerCase)

    val grouped = seq.foldLeft(Seq[(String, Seq[String])]()) { (res, cur) =>
      if isDashedName(cur) || cur == "--" then res :+ (cur -> Seq[String]())
      else res.updateLastOr { case (name, values) => (name, values :+ cur) }((cur, Seq[String]()))
    }
    grouped.flatMap {
      case (name, values) if isDashedName(name) =>
        Seq(name.trimStart("-"), values.mkString(" ").splitQuoted.mkString(" "))
      case (name, values) if name == "--" => values
      case (name, values)                 => Seq(name) ++ values
    }

private[specs2] object Split extends Split
