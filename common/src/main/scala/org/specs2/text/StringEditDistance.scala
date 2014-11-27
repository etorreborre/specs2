package org.specs2
package text

import Split._
import data._
import EditDistance._

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
trait StringEditDistance extends DiffShortener {

  def editDistance(s1: String, s2: String): Int =
    foldSplittedStrings(s1, s2, 0, (r: Int, s1: String, s2: String) => {
      val matrix = new EditMatrix(s1, s2, StringLevenhsteinCosts)
      math.max(r, matrix.operations.count { case Same(_) => false; case _ => true })
    })

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
      val matrix = new data.EditDistance.EditMatrix(s1, s2, StringLevenhsteinCosts)
      val operations1 = matrix.operations
      val operations2 = operations1.map(_.inverse)
      val (diffs1, diffs2) = (showDiffs(operations1, sep, shortenSize), showDiffs(operations2, sep, shortenSize))
      def skipLine(s: String) = if (s.isEmpty) s else s+"\n"
      (skipLine(r._1) + diffs1, skipLine(r._2) + diffs2)
    })

  def showDiffs(operations: IndexedSeq[EditDistanceOperation[Char]], sep: String, shortenSize: Int): String = {
    val delimiter = StringDelimiter(sep)

    val (isDifferent, result) = operations.foldLeft((false, new StringBuilder)) { case ((different, res), op) =>
      if (different) {
        op match {
          case Add(t)     => (true,  res)
          case Del(t)     => (true,  res.append(t))
          case Subst(t,_) => (true,  res.append(t))
          case Same(t)    => (false, res.append(delimiter.second).append(t))
        }
      } else {
        op match {
          case Add(t)     => (true,  res.append(delimiter.first))
          case Del(t)     => (true,  res.append(delimiter.first).append(t))
          case Subst(t,_) => (true,  res.append(delimiter.first).append(t))
          case Same(t)    => (false, res.append(t))
        }
      }
    }
    val fullResult =
      if (isDifferent) result.append(delimiter.second).toString
      else             result.toString

    shorten(fullResult, delimiter.first, delimiter.second, shortenSize)
  }

  /** apply edit distance functions on strings splitted on newlines so that there are no memory issues */
  def foldSplittedStrings[T](s1: String, s2: String, init: T, f: (T, String, String) => T): T = {
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


  /**
   * Create separator characters
   */
  case class StringDelimiter(separator: String) {
    val middle = separator.size / 2 + separator.size % 2
    val first =  if (separator.isEmpty) "" else separator.substring(0, middle)
    val second = if (separator.size < 2) first else separator.substring(middle, separator.size)
    val separators = (first, second)
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

  private def sepList(s: String, firstSep: String, secondSep: String) = {
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

object StringEditDistance extends StringEditDistance

