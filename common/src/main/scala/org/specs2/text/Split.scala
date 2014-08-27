package org.specs2
package text

import Trim._
import collection.Seqx._

private[specs2]
trait Split {

  implicit class Splitted(s: String) {

    def splitToSize(n: Int): List[String] = splitToSize(s, n, Nil)

    private def splitToSize(string: String, n: Int, result: List[String]): List[String] = {
      if (string.size <= n) (string :: result).reverse
      else
        // new Strings are necessary to avoid memory errors because substring is just a view on the underlying string
        splitToSize(new String(string.drop(n)), n, new String(string.take(n)) :: result)
    }

    def splitQuoted = {
      val quoted = "\"[^\"]*\"|[^\\s]+".r
      quoted.findAllIn(s).toSeq.map(_.trimEnclosing("\""))
    }

    /**
     * split a string along some names which start with a dash:
     *
     * "-include hello world -with me".splitDashed(Seq("include", "with")) === ("include", "hello world", "with", "me")
     */
    def splitDashed(names: Seq[String]) = {
      val dashedNames = names.map("-"+_.toLowerCase)
      def isDashedName(name: String) = dashedNames.contains(name.toLowerCase)

      val grouped = s.split("\\s").foldLeft(Seq[(String, Seq[String])]()) { (res, cur) =>
        if (isDashedName(cur) || cur == "--") res :+ (cur -> Seq[String]())
        else                                  res.updateLastOr { case (name, values) => (name, values :+ cur) }((cur, Seq[String]()))
      }
      grouped.flatMap {
        case (name, values) if isDashedName(name) => Seq(name.trimStart("-"), values.mkString(" ").splitQuoted.mkString(" "))
        case (name, values) if name == "--"       => values
        case (name, values)                       => Seq(name) ++ values
      }
    }
  }
}

private[specs2]
object Split extends Split
