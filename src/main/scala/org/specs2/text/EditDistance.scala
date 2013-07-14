package org.specs2
package text

import Trim._
import Split._
/**
 * The EditDistance trait provides methods to compute and display the shortest distance between 2 strings.<p>
 * Usage:<pre>
 * showDistance("kitten", "sitting") // returns ("(k)itt(e)n", "(s)itt(i)n(g)")
 *
 * // with different separators
 * showDistance("kitten", "sitting", "[]") // returns ("[k]itt[e]n", "[s]itt[i]n[g]")
 * </pre>
 */
private[specs2]
trait EditDistance {
  /**
   * Class encapsulating the functions related to the edit distance of 2 strings
   */
  case class EditMatrix(s1: String, s2: String) {
    /* matrix containing the edit distance for any prefix of s1 and s2: matrix(i)(j) = edit distance(s1[0..i], s[0..j])*/
    val matrix = Array.ofDim[Int](s1.length + 1, s2.length + 1)

    /* initializing the matrix */
    for (i <- 0 to s1.length;
         j <- 0 to s2.length) {
      if (i == 0) matrix(i)(j) = j // j insertions
      else if (j == 0) matrix(i)(j) = i  // i suppressions
      else matrix(i)(j) = min(matrix(i - 1)(j) + 1, // suppression
                              matrix(i - 1)(j - 1) + (if (s1(i - 1) == s2(j - 1)) 0 else 1), // substitution
                              matrix(i)(j - 1) + 1) // insertion

    }
    /** @return the edit distance between 2 strings */
    def distance = matrix(s1.length)(s2.length)

    /** prints the edit matrix of 2 strings */
    def print = {
      for (i <- 0 to s1.length) {
        def row = for (j <- 0 to s2.length) yield matrix(i)(j)
        println(row.mkString("|"))
      }
      this
    }

    /** @return a (String, String) displaying the differences between each input strings. The used separators are parenthesis: '(' and ')'*/
    def showDistance: (String, String) = showDistance("()", 20)

    /**
     * @param sep separators used to hightlight differences. If sep is empty, then no separator is used. If sep contains
     * one character, it is taken as the unique separator. If sep contains 2 or more characters, half of them are taken for the opening separator and
     * the rest for the closing separator.
     *
     * @return a (String, String) displaying the differences between each input strings. The used separators are specified by the caller.<p>
     */
    def showDistance(sep: String, shortenSize: Int) = {
      val (firstSeparator, secondSeparator) = separators(sep)
      def modify(s: String, c: Char): String = modifyString(s, c.toString)
      def modifyString(s: String, mod: String): String = (firstSeparator + mod + secondSeparator + s).removeAll(secondSeparator + firstSeparator)

      def findOperations(dist: Int, i: Int, j:Int, s1mod: String, s2mod: String): (String, String) = {
        if (i == 0 && j == 0) {
          ("", "")
        }
        else if (i == 1 && j == 1) {
          if (dist == 0) (s1(0) + s1mod, s2(0) + s2mod)
          else (modify(s1mod, s1(0)), modify(s2mod, s2(0)))
        }
        else if (j < 1) (modifyString(s1mod, s1.slice(0, i)), modifyString(s2mod, ""))
        else if (i < 1) (modifyString(s1mod, ""), modifyString(s2mod, s2.slice(0, j)))
        else {
          val (suppr, subst, ins) = (matrix(i - 1)(j), matrix(i - 1)(j - 1), matrix(i)(j - 1))
          if (suppr < subst)
            findOperations(suppr, i - 1, j, modify(s1mod, s1(i - 1)), modifyString(s2mod, ""))
          else if (ins < subst)
            findOperations(ins, i, j - 1, modifyString(s1mod, ""), modify(s2mod, s2(j - 1)))
          else if (subst < dist)
            findOperations(subst, i - 1, j - 1, modify(s1mod, s1(i - 1)), modify(s2mod, s2(j - 1)))
          else
            findOperations(subst, i - 1, j - 1, s1(i - 1) + s1mod, s2(j - 1) + s2mod)
        }
      }
      val (s1diffs, s2diffs) = findOperations(distance, s1.length, s2.length, "", "")
      import DiffShortener._
      (shorten(s1diffs, firstSeparator, secondSeparator, shortenSize), shorten(s2diffs, firstSeparator, secondSeparator, shortenSize))
    }
    def min(suppr: Int, subst: Int, ins: =>Int) = {
      if(suppr < subst) suppr
      else if (ins < subst) ins
      else subst
    }
  }
  /** @return the edit distance between 2 strings = the minimum number of insertions/suppressions/substitutions to pass from one string to the other */
  def editDistance(s1: String, s2: String): Int = foldSplittedStrings(s1, s2, 0, {
    (r: Int, s1: String, s2: String) => r + new EditMatrix(s1, s2).distance }
  )

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
  /** prints on the console the edit matrix for 2 strings */
  def showMatrix(s1: String, s2: String) = foldSplittedStrings(s1, s2, (), {
    (r: Any, s1: String, s2: String) => new EditMatrix(s1, s2).print }
  )

  /**
   * @param sep separators used to hightlight differences. If sep is empty, then no separator is used. If sep contains
   * one character, it is taken as the unique separator. If sep contains 2 or more characters, the first half of the characters are taken as
   * opening separator and the second half as closing separator.
   *
   * @return a (String, String) displaying the differences between each input strings.
   * The used separators are specified by the caller. The string is shortened before and after differences if necessary. <p>
   */
  def showDistance(s1: String, s2: String, sep: String = "[]", shortenSize: Int = 20): (String, String) = {
    foldSplittedStrings(s1, s2, ("", ""), { (r: (String, String), s1: String, s2: String) =>
        val showDistance = EditMatrix(s1, s2).showDistance(sep, shortenSize)
        def skipLine(s: String) = if (s.isEmpty) s else (s + "\n")
        (skipLine(r._1) + showDistance._1, skipLine(r._2) + showDistance._2)
      }
    )
  }

  private def separators(s: String) = (firstSeparator(s), secondSeparator(s))
  private def firstSeparator(s: String) = if (s.isEmpty) "" else s.substring(0, s.size / 2 + s.size % 2)
  private def secondSeparator(s: String) = if (s.size < 2) firstSeparator(s) else s.substring(s.size / 2 + s.size % 2, s.size)
}

/**
 * This object help shortening strings between differences when the strings are too long
 */
private[specs2]
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
private[specs2]
object DiffShortener extends DiffShortener

private[specs2]
object EditDistance extends EditDistance

