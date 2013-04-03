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

  /**
   * collect the start index of each piece of text in the stringContent
   */
  private def textOffsets(ts: Seq[String]) = {
    ts.foldLeft(((stringContent, ""), Seq[Int]())) { (res, cur) =>
      val ((remaining, passed), result) = res

      // if the current piece of text is empty we need to know if it was the first one of the list
      // otherwise it means that all the remaining string must be consumed
      val i =
        if (cur.isEmpty && ts.head != cur) remaining.size
        else                               remaining.indexOf(cur)

      ((remaining.substring(i + cur.size), passed + remaining.substring(0, i + cur.size)), result :+ (passed.size + i))
    }._2
  }
  // start offsets for text elements in the full content
  private def textStartOffsets = textOffsets(texts)
  // end offsets for text elements in the full content
  private def textEndOffsets = (textStartOffsets zip texts).map { case (start, t) => start + t.size }

  private val trimExpression = (e: String) => {
    if (e.trim.startsWith("${")) e.removeFirst("\\Q${\\E").removeLast("\\Q}\\E")
    else                         e.removeFirst("\\Q$\\E").removeLast("\\Q}\\E")
  }

}
