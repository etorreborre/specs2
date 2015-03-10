package org.specs2
package text

/**
 *  various functions for working out indentation
 */
object Indent {

  def indentAllButFirstLine(text: String, spaces: String): String = {
    val lines = text.split("\n", -1)
    val firstLine = lines.headOption
    val rest = lines.drop(1)
    (firstLine.toSeq ++ rest.toList.map(spaces + _)).mkString("\n")
  }

  /** @return the spaces indenting the last line of text */
  def lastLineIndentation(text: String): String =
    text.split("\n", -1).lastOption.map(_.takeWhile(_ == ' ')).getOrElse("")
}
