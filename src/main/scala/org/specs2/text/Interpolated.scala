package org.specs2
package text

import control.Exceptions._
import text.Trim._

/**
 * this class extracts interpolated expressions from an interpolated string, given the string content and the text
 * pieces in between the interpolated expressions
 */
class Interpolated(stringContent: String, texts: Seq[String]) {

  def expressions = (textEndOffsets.dropRight(1) zip textStartOffsets.drop(1)).map { case (start, end) =>
      tryo(stringContent.substring(start, end)).getOrElse("failed to retrieve text between: "+(start, end))
    }.map(trimExpression)

  private def textOffsets(ts: Seq[String]) = {
    ts.foldLeft(((stringContent, ""), Seq[Int]())) { (res, cur) =>
      val ((remaining, passed), result) = res
      val i = remaining.indexOf(cur)
      ((remaining.substring(i + cur.size), passed + remaining.substring(0, i + cur.size)), result :+ (passed.size + i))
    }._2
  }
  // start offsets for text elements in the full content
  private def textStartOffsets = textOffsets(texts)
  // end offsets for text elements in the full content
  private def textEndOffsets = (textStartOffsets zip texts).map { case (start, t) => start + t.size - 1 }

  private val trimExpression = (e: String) => {
    if (e.trim.startsWith("${")) e.removeFirst("\\Q${\\E").removeLast("\\Q}\\E")
    else                         e.removeFirst("\\Q$\\E").removeLast("\\Q}\\E")
  }

}
