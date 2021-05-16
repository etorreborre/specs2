package org.specs2
package text

import NotNullStrings.*
import scala.collection.Traversable
import collection.canEqualAny

/**
 * Quote and unquote strings
 */
private[specs2]
trait Quote:

  /** quote a value, unless it is a collection of objects */
  def q(a: Any): String =
    if a == null then quote("null")
    else
      a.asInstanceOf[Matchable] match
        case option: Option[?]      => quote(option.notNull)
        case ar: Array[?]           => ar.notNull
        case map: Map[?,?]          => map.notNull
        case it: TraversableOnce[?] => it.notNull
        case _                      => quote(a.notNull)

  /** quote a sequence, with commas if short, with newlines otherwise */
  def qseq(seq: Traversable[?]): String =
    val withCommas = q(seq.mkString(", "))
    if withCommas.length < 30 then withCommas
    else seq.mkString("\n", "\n  ", "\n")

  /** quote a string */
  def quote(s: String, addQuotes: Boolean = true) = if addQuotes then "'"+s+"'" else s

  /** @return an object.toString() without quotes (used in messages creation) */
  def unq(a: Any)  = a.notNull

  extension (s: String)
    def prefix(separator: String, other: String): String =
      Seq(s, other).filter(_.nonEmpty).mkString(separator)

private[specs2]
object Quote extends Quote
