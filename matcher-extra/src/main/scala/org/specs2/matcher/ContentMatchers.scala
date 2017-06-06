package org.specs2
package matcher

import java.io.File

import text._
import AnsiColors._
import io._
import MatchResult._
import scalaz.Scalaz._

/**
 * The ContentMatchers trait provides matchers to make comparisons between files, sequences,...
 */
trait ContentMatchers extends LinesContentMatchers with FilesContentMatchers
object ContentMatchers extends ContentMatchers

trait LinesContentMatchers extends LinesContentBaseMatchers with LinesContentBeHaveMatchers
object LinesContentMatchers extends LinesContentMatchers

private[specs2]
trait LinesContentBaseMatchers extends DifferenceFilters with Expectations with SeqsContents {

  /** matches if 2 contents, as a pair, have the same lines */
  def haveSameLines[L1 : LinesContent, L2 : LinesContent]: LinesPairComparisonMatcher[L1, L2] =
    LinesPairComparisonMatcher[L1, L2]()

  /** matches if 2 contents have the same lines */
  def haveSameLinesAs[L1 : LinesContent, L2 : LinesContent](ls2: L2): LinesComparisonMatcher[L1, L2] =
    LinesComparisonMatcher[L1, L2](ls2)

  /** matches if 1 lines content contains the lines of the other one */
  def containLines[L1 : LinesContent, L2 : LinesContent](ls2: L2): LinesComparisonMatcher[L1, L2] =
    LinesComparisonMatcher[L1, L2](ls2, all = false)

  // default implementation for reading file lines
  implicit protected val fileContentForMatchers: LinesContent[File] = FileLinesContent

  /**
   * Matcher to compare the contents of line contents
   */
  case class LinesComparisonMatcher[L1 : LinesContent, L2 : LinesContent](
    ls2: L2,
    all: Boolean = true,
    ordered: Boolean = true,
    colors: Boolean = true,
    filter: DifferenceFilter = DifferencesClips()) extends Matcher[L1] {

    def apply[S <: L1](t: Expectable[S]): MatchResult[S] = {
      val ls1 = t.value
      val (content1, content2) = (implicitly[LinesContent[L1]], implicitly[LinesContent[L2]])

      val (name1, name2) = (content1.name(ls1), content2.name(ls2))
      val (n1, n2) = if (name1 == name2) ("the first "+name1, "the second "+name1) else (name1, name2)

      val diffs = content1.differences(ls1, ls2, all, ordered)

      result(diffs.isEmpty,
             n1+" "+okMessage+" "+n2,
             n1+" "+koMessage+" "+n2+"\n"+
             showDiffs(filter(diffs.show))+"\n",
             t)
    }

    def showWith(show: DifferenceFilter) = copy[L1, L2](filter = show)
    def unordered                        = copy[L1, L2](ordered = false)
    def nocolors                         = copy[L1, L2](colors = false)

    protected def showDiffs(s: Seq[_]): String =
      s.flatMap {
        case SameLine(NumberedLine(n, l))    => List(s"  $n. $l")
        case AddedLine(NumberedLine(n, l))   => List(color(s"+ $n. $l", green))
        case DeletedLine(NumberedLine(n, l)) => List(color(s"- $n. $l", red))

        case DifferentLine(NumberedLine(n1, l1), NumberedLine(n2, l2)) =>
          List(color(s"+ $n1. $l1", green), color(s"- $n2. $l2", red))
      }.map("    "+_).mkString("", "\n", "\n")

    protected def okMessage =
      if (all) "is the same as"
      else     "contains"

    protected def koMessage = {
      if (all) "is not the same as"
      else     "does not contain"
    }

  }

  case class LinesPairComparisonMatcher[L1 : LinesContent, L2 : LinesContent](all: Boolean = true,
                                                                              ordered: Boolean = true,
                                                                              colors: Boolean = true,
                                                                              filter: DifferenceFilter = DifferencesClips())
    extends Matcher[(L1, L2)] {

    def apply[S <: (L1, L2)](t: Expectable[S]): MatchResult[S] = {
      val (ls1, ls2) = t.value
      new LinesComparisonMatcher[L1, L2](ls2, all, ordered, colors, filter).
        apply(createExpectable(ls1)).map((_:L1) => t.value)
    }

    def showOnly(show: DifferenceFilter) = copy[L1, L2](filter = show)
    def unordered                        = copy[L1, L2](ordered = false)
    def nocolors                         = copy[L1, L2](colors = false)
  }

}

/**
 * This trait is mostly extracted for implicit search reasons.
 *
 * Otherwise in the expression (f1, f2) must haveSameLines, the implicit for Se[String] would conflict with this one
 */
private[specs2]
trait SeqsContents {
  // default implementation for reading seq lines
  implicit protected def seqContentForMatchers[T, CC[_] <: Traversable[_]]: LinesContent[CC[T]] =
    SeqLinesContent[T, CC]()

}

private[specs2]
trait LinesContentBeHaveMatchers extends BeHaveMatchers { this: LinesContentBaseMatchers =>
  /**
   * matcher aliases and implicits to use with BeVerb and HaveVerb
   */
  implicit def toLinesContentResultMatcher[L1 : LinesContent](result: MatchResult[L1]): LinesContentResultMatcher[L1] =
    new LinesContentResultMatcher(result)

  class LinesContentResultMatcher[L1 : LinesContent](result: MatchResult[L1]) {
    def sameLinesAs[L2: LinesContent](ls2: L2) = result(haveSameLinesAs[L1, L2](ls2))
  }

  def sameLinesAs[L1 : LinesContent, L2 : LinesContent](ls2: L2) = haveSameLinesAs[L1, L2](ls2)

}
