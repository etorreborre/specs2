package org.specs2
package text

import Trim._
import Split._
/**
 * The EditDistance trait provides methods to compute and display the shortest distance between 2 strings.
 *
 * Usage:<pre>
 * showDistance("kitten", "sitting") // returns ("(k)itt(e)n", "(s)itt(i)n(g)")
 *
 * // with different separators
 * showDistance("kitten", "sitting", "[]") // returns ("[k]itt[e]n", "[s]itt[i]n[g]")
 * </pre>
 */
trait EditDistance {
  type OpType = String
  lazy val INS = "INS"
  lazy val DEL = "DEL"
  lazy val SUBST = "SUBST"
  lazy val NONE = "NONE"

  case class Op(opType: OpType, cost: Int = 0) {
    override def toString =
      (if      (opType == INS)   "+"
       else if (opType == DEL)   "-"
       else if (opType == SUBST) "~"
       else                      " ")+cost
  }
  type DistanceMatrix = Array[Array[Op]]

  /**
   * Edit matrix for 2 given strings
   */
  class EditMatrix(s1: String, s2: String) extends StringDistance with ShowDistance with DiffShortener {
    /* matrix containing the edit distance for any prefix of s1 and s2: matrix(i)(j) = edit distance(s1[0..i], s[0..j])*/
    private val matrix = Array.ofDim[Op](s1.length + 1, s2.length + 1)
    initialize(s1, s2, matrix)

    /** @return the edit distance between 2 strings */
    def distance = lastOp.cost
    private def lastOp = matrix(s1.length)(s2.length)

    /** prints the edit matrix of the 2 strings */
    def showMatrix = {
      val result = new StringBuilder

      for (i <- 0 to s1.length) {
        def row = for (j <- 0 to s2.length) yield matrix(i)(j)
        result.append(row.mkString("|", "|", "|\n"))
      }
      result.toString
    }

    /** show the distance between 2 strings */
    def showDistance: (String, String) = showDistance("[]", 20)

    /** show the distance between 2 strings by enclosing them with separators */
    def showDistance(separator: String, shortenSize: Int): (String, String) = {
      val (firstSeparator, secondSeparator) = StringDelimiter(separator).separators
      val (s1diffs, s2diffs) = showOperations(s1, s2, matrix, separator)
      (shorten(s1diffs, firstSeparator, secondSeparator, shortenSize),
       shorten(s2diffs, firstSeparator, secondSeparator, shortenSize))
    }

    /** @return the longest common subsequence of letters between the 2 strings */
    def longestSubsequence = showDistance._1.replaceAll("\\[.*?\\]", "")

    override def toString = showMatrix
  }

  /**
   * Evaluate the distance between 2 strings by counting the number of insertions, suppressions or substitutions
   * which are necessary to transform one into the other
   */
  trait StringDistance {

    def initialize(s1: String, s2: String, matrix: DistanceMatrix): Unit =
      for (i <- 0 to s1.length;
           j <- 0 to s2.length) {
        if (i == 0)      matrix(i)(j) = Op(INS, j)                 // j insertions
        else if (j == 0) matrix(i)(j) = Op(DEL, i)                 // i suppressions
        else             matrix(i)(j) = cost(s1, s2, i, j, matrix) // otherwise
      }

    /** @return the cost of a substitution */
    def substitutionCost(a: Char, b: Char): Int = if (a == b) 0 else 1

    /** @return the cost of an insertion or deletion */
    def insertionDeletionCost(c: Char) = 1

    /**
     * @return the lower cost and associated operation of a deletion, substitution or insertion
     *         in case of equality between a non-substitution and an insertion/suppression
     *         we select the insertion/suppression in order to group all the differences together
     *         diff("abcd", "acbd") ==> ("a[bc]d", "a[cb]d"). the distance is 2, otherwise
     *         diff("abcd", "acbd") ==> ("a[b]c[]d", "a[c]b[]d")
     */
    def lowerCost(a: Char, b: Char, del: Int, subst: Int, ins: Int): Op = {
      val (opDel, opSubst, opIns) = (Op(DEL, del), Op(SUBST, subst), Op(INS, ins))
      if (ins < del) {
        if (ins < subst) opIns
        else if (ins == subst && a == b) opIns
        else opSubst
      } else {
        if (del < subst) opDel
        else if (del == subst && a == b) opDel
        else opSubst
      }
    }

    /** @return the cost for DistanceMatrix(i, j) */
    def cost(s1: String, s2: String, i: Int, j: Int, matrix: DistanceMatrix) = {
      val result = lowerCost(s1(i - 1), s2(j - 1),
         matrix(i - 1)(j).cost     + insertionDeletionCost(s1(i - 1)),       // suppression
         matrix(i - 1)(j - 1).cost + substitutionCost(s1(i - 1), s2(j - 1)), // substitution
         matrix(i)(j - 1).cost     + insertionDeletionCost(s2(j - 1)))       // insertion

      if (result.opType == SUBST && matrix(i - 1)(j - 1).cost == result.cost) Op(NONE, result.cost)
      else result
    }
  }

  /**
   * Given 2 strings and their edit distance matrix, show the operations allowing to go from one string to the other
   */
  trait ShowDistance extends DiffShortener {
    /**
     * separators are used to highlight differences. If sep is empty, then no separator is used. If sep contains
     * one character, it is taken as the unique separator. If sep contains 2 or more characters, half of them are taken for the opening separator and
     * the rest for the closing separator.
     *
     * @return a (String, String) displaying the differences between each input strings. The used separators are specified by the caller.<p>
     */
    protected def showOperations(s1: String, s2: String, matrix: DistanceMatrix, separator: String) = {
      val delimiter = StringDelimiter(separator)

      def showAllOperations(i: Int, j:Int, s1Operations: String = "", s2Operations: String = ""): (String, String) = {
        if (i == 0 && j == 0) ("", "")
        else {
          val op = matrix(i)(j)
          val dist = op.cost
          if (i == 1 && j == 1) {
            if (dist == 0) (s1(0) + s1Operations,                          s2(0) + s2Operations)
            else           (delimiter.add(s1(0), s1Operations),            delimiter.add(s2(0), s2Operations))
          }
          else if (j < 1) (delimiter.append(s1.slice(0, i), s1Operations), delimiter.appendEmpty(s2Operations))
          else if (i < 1) (delimiter.appendEmpty(s1Operations),            delimiter.append(s2.slice(0, j), s2Operations))
          else {
            if      (op.opType == INS)   showAllOperations(i,     j - 1, delimiter.appendEmpty(s1Operations),    delimiter.add(s2(j - 1), s2Operations))
            else if (op.opType == DEL)   showAllOperations(i - 1, j,     delimiter.add(s1(i - 1), s1Operations), delimiter.appendEmpty(s2Operations))
            else if (op.opType == SUBST) showAllOperations(i - 1, j - 1, delimiter.add(s1(i - 1), s1Operations), delimiter.add(s2(j - 1), s2Operations))
            else                         showAllOperations(i - 1, j - 1, s1(i - 1) + s1Operations,               s2(j - 1) + s2Operations)
          }
        }
      }
      val (diff1, diff2) = showAllOperations(s1.length, s2.length)
      (delimiter.trim(diff1), delimiter.trim(diff2))
    }
  }

  /**
   * reconstruct strings by appending modified elements with separators
   */
  case class StringDelimiter(separator: String) {
    lazy val separators = (first, second)

    def add(c: Char, s: String): String = append(c.toString, s)
    def append(s2: String, s1: String): String = first+s2+second+s1
    def appendEmpty(s: String): String = append("", s)

    def trim(s: String) = s.removeAll(second+first)

    private val middle = separator.size / 2 + separator.size % 2
    private lazy val first =  if (separator.isEmpty) "" else separator.substring(0, middle)
    private lazy val second = if (separator.size < 2) first else separator.substring(middle, separator.size)
  }

  /** @return the edit distance between 2 strings = the minimum number of insertions/suppressions/substitutions to pass from one string to the other */
  def editDistance(s1: String, s2: String): Int =
    foldSplittedStrings(s1, s2, 0, (r: Int, s1: String, s2: String) => { r + new EditMatrix(s1, s2).distance })

  /** show the edit matrix for 2 strings */
  def showMatrix(s1: String, s2: String) =
    foldSplittedStrings(s1, s2, (), (r: Any, s1: String, s2: String) => new EditMatrix(s1, s2).showMatrix)

  /**
   * @param sep separators used to hightlight differences. If sep is empty, then no separator is used. If sep contains
   * one character, it is taken as the unique separator. If sep contains 2 or more characters, the first half of the characters are taken as
   * opening separator and the second half as closing separator.
   *
   * @return a (String, String) displaying the differences between each input strings.
   * The used separators are specified by the caller. The string is shortened before and after differences if necessary. <p>
   */
  def showDistance(s1: String, s2: String, sep: String = "[]", shortenSize: Int = 20): (String, String) =
    foldSplittedStrings(s1, s2, ("", ""), (r: (String, String), s1: String, s2: String) => {
      val showDistance = new EditMatrix(s1, s2).showDistance(sep, shortenSize)
      def skipLine(s: String) = if (s.isEmpty) s else s+"\n"
      (skipLine(r._1) + showDistance._1, skipLine(r._2) + showDistance._2)
    })

  /** apply edit distance functions on strings splitted on newlines so that there are no memory issues */
  private def foldSplittedStrings[T](s1: String, s2: String, init: T, f: (T, String, String) => T): T = {
    val (splitted1, splitted2) = split(s1, s2)
    splitted1.zip(splitted2).foldLeft(init) { (result, current) =>
      f(result, current._1, current._2)
    }
  }

  /**
   * In order to avoid CPU and memory issues, split the 2 strings to compare:
   *  - along newlines. This is useful if the 2 strings represent a file content
   *  - otherwise split the strings so that they are less than 200 characters long
   */
  private def split(s1: String, s2: String): (List[String], List[String]) = {
    val (splitted1, splitted2) = (s1.split("\n").toList, s2.split("\n").toList)
    def splitToSize(strings: List[String]) = strings.flatMap(_.splitToSize(200))
    (splitToSize(splitted1), splitToSize(splitted2))
  }
}

/**
 * This object help shortening strings between differences when the strings are too long
 */
trait DiffShortener {
  def shorten(s: String, firstSep: String = "[", secondSep: String = "]", size: Int = 5): String = {
    def shortenLeft(s: String) = if (s.size > size) ("..." + s.slice(s.size - size, s.size)) else s
    def shortenRight(s: String) = if (s.size > size) (s.slice(0, size) + "...") else s
    def shortenCenter(s: String) = if (s.size > size) (s.slice(0, size / 2) + "..." + s.slice(s.size - size / 2, s.size)) else s
    val list = sepList(s, firstSep, secondSep)
    list.foldLeft("") { (res, cur) =>
      if (cur.startsWith(firstSep) && cur.endsWith(secondSep))
        res + cur
      else if (list.head eq cur)
        res + shortenLeft(cur)
      else if (list.last eq cur)
        res + shortenRight(cur)
      else
        res + shortenCenter(cur)
    }
  }
  def sepList(s: String, firstSep: String, secondSep: String) = {
    def split(s: String, sep: String): Array[String] =
      if (List("[", "]" ,"(", ")", "-", "+", "?", "*").contains(sep)) split(s, "\\" + sep) else s.split(sep)

    val splitted = split(s, firstSep)
    if (splitted.size == 1) List(s)
    else {
      splitted.foldLeft(List[String]()) { (res, cur) =>
        if (!cur.contains(secondSep)) res :+ cur
        else {
          lazy val diff = split(cur, secondSep)(0)
          if (split(cur, secondSep).size == 0)
            res :+ (firstSep + secondSep)
          else if (split(cur, secondSep).size > 1)
            res ++ List(firstSep + diff + secondSep, split(cur, secondSep)(1))
          else
            res :+ (firstSep + diff + secondSep)
        }
      }
    }
  }
}
object DiffShortener extends DiffShortener

object EditDistance extends EditDistance

