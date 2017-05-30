package org.specs2
package text

import collection._
import Seqx._
import LineComparison._
import org.specs2.control.producer._, Producer._
import org.specs2.control.eff._
import org.specs2.control.eff.syntax.safe._
import org.specs2.control.eff.syntax.eff._

/**
 * This trait represents the difference between 2 "contents"
 */
trait ContentDifference {
  type Difference

  /** @return true if there is no difference in 2 contents */
  def isEmpty: Boolean

  /** @return a list of differences between content1 and content2 */
  def show: Seq[Difference]
}

/**
 * This class shows the differences between 2 sequences of lines.
 *
 * - if all is false we expect possibly more lines in lines2
 * - if ordered is false we expect the lines of lines1 to appear in any order in lines2
 */
case class LinesContentDifference(
  lines1: Seq[String], lines2: Seq[String],
  all: Boolean = true,
  ordered: Boolean = true) extends ContentDifference {

  val numberedLines1 = lines1.toIndexedSeq.zipWithIndex.map { case (l, i) => NumberedLine(i + 1, l) }
  val numberedLines2 = lines2.toIndexedSeq.zipWithIndex.map { case (l, i) => NumberedLine(i + 1, l) }

  type Difference = LineComparison
  private type Diffs = Seq[Difference]

  def isEmpty =
    show.forall { case SameLine(_) => true; case _ => false }

  lazy val show: Diffs = {
    if      (all  && ordered)  showNotEqual
    else if (all  && !ordered) showNotOrdered
    else if (!all && ordered)  showNotIncluded
    else                       showNotContained
  }

  // all && ordered
  private lazy val showNotEqual: Diffs = {
    import org.specs2.data._
    import EditDistance._

    val operations: IndexedSeq[EditDistanceOperation[NumberedLine]] =
      levenhsteinDistance(numberedLines1, numberedLines2)

    operations.flatMap {
      case Same(line)          => List(SameLine(line))
      case Add(line)           => List(DeletedLine(line))
      case Del(line)           => List(AddedLine(line))
      case Subst(line1, line2) => List(DifferentLine(line1, line2))
    }
  }

  // all && unordered
  private lazy val showNotOrdered: Diffs =
    (numberedLines1.intersect(numberedLines2).                  map(l => sameLine(l)) ++
     numberedLines1.delta[NumberedLine](numberedLines2, _ == _).map(l => addedLine(l)) ++
     numberedLines2.delta[NumberedLine](numberedLines1, _ == _).map(l => deletedLine(l))).sorted

  // partial && ordered
  private lazy val showNotIncluded: Diffs =
    LinesContentDifference(lines1 filter lines2.contains, lines2, all = true, ordered).show

  // partial && unordered
  private lazy val showNotContained: Diffs  =
    LinesContentDifference(lines1 filter lines2.contains, lines2, all = true, ordered).show

}

/**
 * case classes for the representation of lines which are different: not found, missing, misplaced
 */
sealed trait LineComparison {
  def line: NumberedLine
  def isDifference: Boolean = true
}
case class SameLine(line: NumberedLine) extends LineComparison {
  override def isDifference = false
}
case class AddedLine(line: NumberedLine) extends LineComparison
case class DeletedLine(line: NumberedLine) extends LineComparison
case class DifferentLine(line1: NumberedLine, line2: NumberedLine) extends LineComparison {
  def line = line1
}

object LineComparison {

  implicit def lineComparisonOrdering: Ordering[LineComparison] = new Ordering[LineComparison] {
    def compare(x: LineComparison, y: LineComparison): Int =
      NumberedLine.numberedLineOrdering.compare(x.line, y.line)
  }

  def sameLine(line: NumberedLine): LineComparison = SameLine(line)
  def addedLine(line: NumberedLine): LineComparison = AddedLine(line)
  def deletedLine(line: NumberedLine): LineComparison = DeletedLine(line)
  def differentLine(line1: NumberedLine, line2: NumberedLine): LineComparison = DifferentLine(line1, line2)

  def clipDifferences(differences: Seq[LineComparison], clipSize: Int): Seq[LineComparison] = {
    val diffs = differences.toList

    emit(diffs).zipWithPreviousAndNextN(clipSize).flatMap {
      case (before, SameLine(l), after) if (before ++ after).exists(_.isDifference) =>
        one(sameLine(l))

      case (before, l, after) if l.isDifference =>
        one(l)

      case _ =>
        done[Fx1[Safe], LineComparison]
    }.runList.execSafe.run.toOption.getOrElse(List())

  }

}

case class NumberedLine(lineNumber: Int, line: String) {
  override def equals(a: Any): Boolean =
    a match {
      case NumberedLine(_, l) => l == line
      case _                  => false
    }

  override def hashCode = line.hashCode
}

object NumberedLine {
  implicit def numberedLineOrdering: Ordering[NumberedLine] = new Ordering[NumberedLine] {
    def compare(x: NumberedLine, y: NumberedLine): Int =
      x.lineNumber.compare(y.lineNumber)
  }
}

/**
 * A trait to filter results of a difference check
 */
trait DifferenceFilter extends Function1[Seq[LineComparison], Seq[LineComparison]]

/**
 * This trait provides some syntactic sugar to create a DifferenceFilter to take only the first n differences:
 *
 *  10.differences == FirstNDifferencesFilter(10)
 */
trait DifferenceFilters {
  implicit def toDifferenceFilter(n: Int): FirstNDifferencesFilter = FirstNDifferencesFilter(n)
  case class FirstNDifferencesFilter(n: Int) {
    def difference = FirstDifferences(n: Int)
    def differences = FirstDifferences(n: Int)
  }
}

/**
 * mix-in this trait to remove the implicit provided by the DifferenceFilters trait
 */
trait NoDifferenceFilters extends DifferenceFilters {
  override def toDifferenceFilter(n: Int): FirstNDifferencesFilter = super.toDifferenceFilter(n)
}

/**
 * return all the differences
 */
object AllDifferences extends SomeDifferences((s: Seq[LineComparison]) => s)

/**
 * return all only changes + some context
 */
case class DifferencesClips(clipSize: Int = 4) extends DifferenceFilter {
  def apply(diffs: Seq[LineComparison]): Seq[LineComparison] =
    LineComparison.clipDifferences(diffs, clipSize)
}


/**
 * return the first n differences
 */
case class FirstDifferences(n: Int) extends SomeDifferences((s: Seq[LineComparison]) => s.filter(_.isDifference).take(n))

/**
 * return some of the differences, filtered with a function
 */
class SomeDifferences(f: Seq[LineComparison] => Seq[LineComparison]) extends DifferenceFilter {
  def apply(diffs: Seq[LineComparison]) = f(diffs)
}
