package org.specs2
package text

import Trim._

/**
 * This class extracts interpolated expressions from an interpolated string, given the string content and the text
 * pieces in between the interpolated expressions
 */
class Interpolated(stringContent: String, texts: Seq[String]) {

  def expressions = {
    texts.zip(texts.drop(1)).foldLeft((stringContent, Seq[String]())) { case ((content, expressions), (text, next)) =>
      val minusText = new String(content.drop(text.length).mkString).
                        replace("$$", "$") // in next, this replacement has already been done
      val extracted = new String(if (minusText.indexOf(next) > 0) minusText.substring(0, minusText.indexOf(next)) else minusText)
      (new String(minusText.drop(extracted.length)), expressions :+ trimVariableDeclaration(extracted))
    }._2
  }

  private def trimVariableDeclaration(s: String): String =
    s.removeStart("$").removeStart("{").removeEnd("}").removeStart("`").removeEnd("`")
}
