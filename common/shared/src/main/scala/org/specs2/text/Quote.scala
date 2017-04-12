package org.specs2
package text

import NotNullStrings._
import scala.collection.GenTraversableOnce

/**
 * Quote and unquote strings
 */
private[specs2]
trait Quote {

  /** quote a value, unless it is a collection of objects */
  def q(a: Any): String = {
    if (a == null) quote("null")
    else {
      a match {
        case ar: Array[_]           => ar.notNull
        case map: Map[_,_]          => map.notNull
        case it: TraversableOnce[_] => it.notNull
        case _                      => quote(a.notNull)
      }
    }
  }

  /** quote a sequence, with commas if short, with newlines otherwise */
  def qseq(seq: GenTraversableOnce[_]): String = {
    val withCommas = q(seq.mkString(", "))
    if (withCommas.size < 30) withCommas
    else seq.mkString("\n", "\n  ", "\n")
  }

  /** quote a string */
  def quote(s: String, addQuotes: Boolean = true) = if (addQuotes) "'"+s+"'" else s

  /** @return an object.toString() without quotes (used in messages creation) */
  def unq(a: Any)  = a.notNull

  implicit def prefixed(s: String) = new Prefixed(s)
  class Prefixed(s: String) {
    def prefix(separator: String, other: String) = Seq(s, other).filter(_.nonEmpty).mkString(separator)
  }
}

private[specs2]
object Quote extends Quote