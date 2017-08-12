package org.specs2
package specification
package script

/**
 * Set of extracted lines from some text which are either: simple text, given text, when text or then text
 */
case class GivenWhenThenLines(lines: Vector[GWTLines] = Vector()) extends ScriptLines {
  def prepend(ls: GWTLines) = (ls, lines.headOption) match {
    case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = TextLines (l1 ++ l2) +: lines.tail)
    case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = GivenLines(l1 ++ l2) +: lines.tail)
    case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = WhenLines (l1 ++ l2) +: lines.tail)
    case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = ThenLines (l1 ++ l2) +: lines.tail)
    case (TextLines(l1), _)                     => if (l1.nonEmpty) copy(lines = ls +: lines) else this
    case _                                      => copy(lines = ls +: lines)
  }

  def append(ls: GWTLines) =
    (ls, lines.lastOption) match {
      case (TextLines(l1), Some(TextLines(l2)))   => copy(lines = lines.dropRight(1) :+ TextLines(l2 ++ l1))
      case (GivenLines(l1), Some(GivenLines(l2))) => copy(lines = lines.dropRight(1) :+ GivenLines(l2 ++ l1))
      case (WhenLines(l1), Some(WhenLines(l2)))   => copy(lines = lines.dropRight(1) :+ WhenLines(l2 ++ l1))
      case (ThenLines(l1), Some(ThenLines(l2)))   => copy(lines = lines.dropRight(1) :+ ThenLines(l2 ++ l1))
      case (TextLines(l1), _)                     => if (l1.nonEmpty) copy(lines = lines :+ ls) else this
      case _                                      => copy(lines = lines :+ ls)
    }
}

trait GWTLines
case class GivenLines(lines: Vector[String]) extends GWTLines
object GivenLines { def create(line: String): GivenLines = GivenLines(Vector(line)) }

case class WhenLines(lines: Vector[String]) extends GWTLines
object WhenLines { def create(line: String): WhenLines = WhenLines(Vector(line)) }

case class ThenLines(lines: Vector[String]) extends GWTLines
object ThenLines { def create(line: String): ThenLines = ThenLines(Vector(line)) }

case class TextLines(lines: String) extends GWTLines
object TextLines { def create(lines: Seq[String]): TextLines = TextLines(lines.mkString("\n")) }



