package org.specs2
package text

import Trim._
import collection.Seqx._
import util.matching.Regex

private[specs2]
trait Split:

  extension (s: String):
    def splitDashed(names: Seq[String]): Seq[String] =
      splitDashed(s.split("\\s").toIndexedSeq, names)

    def splitQuoted: Seq[String] =
      quoted.findAllIn(s).toSeq.map(_.trimEnclosing("\""))

    def splitToSize(n: Int): List[String] =
      splitToSize(s, n, Nil)

  private val quoted: Regex =
    "\"[^\"]*\"|[^\\s]+".r

  private def splitToSize(string: String, n: Int, result: List[String]): List[String] =
    if string.length <= n then
      (string :: result).reverse
    else
      // new Strings are necessary to avoid memory errors because substring is just a view on the underlying string
      splitToSize(new String(string.drop(n)), n, new String(string.take(n)) :: result)


  extension (seq: Seq[String]):
    def splitDashed(names: Seq[String]): Seq[String] =
      splitDashed(seq, names)

  /**
   * split a string along some names which start with a dash:
   *
   * "-include hello world -with me".splitDashed(Seq("include", "with")) === ("include", "hello world", "with", "me")
   */
  def splitDashed(seq: Seq[String], names: Seq[String]): Seq[String] =
    val dashedNames = names.map("-"+_.toLowerCase)
    def isDashedName(name: String) = dashedNames.contains(name.toLowerCase)

    val grouped = seq.foldLeft(Seq[(String, Seq[String])]()) { (res, cur) =>
      if isDashedName(cur) || cur == "--" then res :+ (cur -> Seq[String]())
      else                                  res.updateLastOr { case (name, values) => (name, values :+ cur) }((cur, Seq[String]()))
    }
    grouped.flatMap {
      case (name, values) if isDashedName(name) => Seq(name.trimStart("-"), values.mkString(" ").splitQuoted.mkString(" "))
      case (name, values) if name == "--"       => values
      case (name, values)                       => Seq(name) ++ values
    }

private[specs2]
object Split extends Split
