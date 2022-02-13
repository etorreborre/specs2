package org.specs2
package text

/**
 * Any type T with a name and representable as a sequence of Strings
 */
trait LinesContent[L1] {

  /** @return a name for this content */
  def name(t: L1): String

  /** @return the lines for this content */
  def lines(t: L1): Seq[String]

  /** @return the differences with another content */
  def differences[L2: LinesContent](ls1: L1, ls2: L2,
                                    all:     Boolean = true,
                                    ordered: Boolean = true): LinesContentDifference =
    LinesContentDifference(lines(ls1), implicitly[LinesContent[L2]].lines(ls2), all, ordered)
}

object LinesContent {
  implicit def seqLinesContent[T]: LinesContent[Seq[T]] =
    SeqLinesContent()
  implicit def listLinesContent[T]: LinesContent[List[T]] =
    ListLinesContent()
}

/**
 * Default implementation for reading lines out of a Seq
 */
case class SeqLinesContent[A]() extends LinesContent[Seq[A]] {
  def name(seq: Seq[A]) = "sequence"
  def lines(seq: Seq[A]): Seq[String] = seq.map(_.toString)
}

case class ListLinesContent[A]() extends LinesContent[List[A]] {
  def name(seq: List[A]) = "list"
  def lines(seq: List[A]): List[String] = seq.map(_.toString)
}
