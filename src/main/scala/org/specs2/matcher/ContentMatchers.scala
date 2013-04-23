package org.specs2
package matcher

import java.io.File
import text._
import io._
import MatchResult._
import scalaz.Scalaz._

/**
 * The ContentMatchers trait provides matchers to make comparisons between files, sequences,...
 */
trait ContentMatchers extends ContentBaseMatchers with ContentBeHaveMatchers
object ContentMatchers extends ContentMatchers

private[specs2]
trait ContentBaseMatchers extends DifferenceFilters with Expectations with SeqsContents {

  /** matches if 2 contents, as a pair, have the same lines */
  def haveSameLines[L1 : LinesContent, L2 : LinesContent]: LinesPairComparisonMatcher[L1, L2] =
    LinesPairComparisonMatcher[L1, L2]()

  /** matches if 2 contents have the same lines */
  def haveSameLinesAs[L1 : LinesContent, L2 : LinesContent](ls2: L2): LinesComparisonMatcher[L1, L2] =
    LinesComparisonMatcher[L1, L2](ls2)

  /** matches if 1 lines content contains the lines of the other one */
  def containLines[L1 : LinesContent, L2 : LinesContent](ls2: L2): LinesComparisonMatcher[L1, L2] =
    LinesComparisonMatcher[L1, L2](ls2, partial = true)

  // default implementation for reading file lines
  implicit protected val fileContentForMatchers: LinesContent[File] = FileLinesContent

  /**
   * Matcher to compare the contents of line contents
   */
  case class LinesComparisonMatcher[L1 : LinesContent, L2 : LinesContent](
    ls2: L2,
    partial: Boolean = false,
    isUnordered: Boolean = false,
    reportMisplaced: Boolean = true,
    filter: DifferenceFilter = AllDifferences) extends Matcher[L1] {

    def apply[S <: L1](t: Expectable[S]): MatchResult[S] = {
      val ls1 = t.value
      val (content1, content2) = (implicitly[LinesContent[L1]], implicitly[LinesContent[L2]])

      val (name1, name2) = (content1.name(ls1), content2.name(ls2))
      val (n1, n2) = if (name1 == name2) ("the first "+name1, "the second "+name1) else (name1, name2)

      val diffs = content1.differences(ls1, ls2, partial, isUnordered, reportMisplaced)
      val (in1Not2, in2Not1) = filter(diffs.show)

      result(diffs.isEmpty,
             n1+" "+okMessage+" "+n2,
             n1+" "+koMessage+" "+n2+"\n"+
             inANotB(n1, n2) + showDiffs(in1Not2)+"\n"+
             inANotB(n2, n1) + showDiffs(in2Not1),
             t)
    }

    def showOnly(show: DifferenceFilter) = copy[L1, L2](filter = show)
    def unordered                        = copy[L1, L2](isUnordered = true)
    def missingOnly                      = copy[L1, L2](reportMisplaced = false)

    protected def showDiffs(s: Seq[_]) = s.map("  "+_).mkString("\n")
    protected def inANotB(n1: String, n2: String) = {
      if (isUnordered || reportMisplaced) "in "+n1+", not in "+n2+"\n"
      else                                "in "+n1+", not in "+n2+" on the same line\n"

    }

    protected def okMessage =
      if (partial) "contains"
      else         "is the same as"

    protected def koMessage = {
      if (partial) "does not contain"
      else         "is not the same as"
    }

  }

  case class LinesPairComparisonMatcher[L1 : LinesContent, L2 : LinesContent](partial: Boolean = false,
                                                                              isUnordered: Boolean = false,
                                                                              reportMisplaced: Boolean = true,
                                                                              filter: DifferenceFilter = AllDifferences)
    extends Matcher[(L1, L2)] {

    def apply[S <: (L1, L2)](t: Expectable[S]): MatchResult[S] = {
      val (ls1, ls2) = t.value
      new LinesComparisonMatcher[L1, L2](ls2, partial, isUnordered, reportMisplaced, filter).
        apply(createExpectable(ls1)).map((_:L1) => t.value)
    }

    def showOnly(show: DifferenceFilter) = copy[L1, L2](filter = show)
    def unordered                        = copy[L1, L2](isUnordered = true)
    def missingOnly                      = copy[L1, L2](reportMisplaced = false)
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
  implicit protected def seqContentForMatchers[T]: LinesContent[Seq[T]] = SeqLinesContent[T]()

}

private[specs2]
trait ContentBeHaveMatchers { this: ContentBaseMatchers =>
  /**
   * matcher aliases and implicits to use with BeVerb and HaveVerb
   */
  implicit def toLinesContentResultMatcher[L1 : LinesContent](result: MatchResult[L1]) = new LinesContentResultMatcher(result)
  class LinesContentResultMatcher[L1 : LinesContent](result: MatchResult[L1]) {
    def sameLinesAs[L2: LinesContent](ls2: L2) = result(haveSameLinesAs[L1, L2](ls2))
  }

  def sameLinesAs[L1 : LinesContent, L2 : LinesContent](ls2: L2) = haveSameLinesAs[L1, L2](ls2)

}
