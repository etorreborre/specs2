package org.specs2
package text

import java.util.regex.Pattern
import Split.*
import data.*
import EditDistance.*
import collection.Seqx.*
import collection.canEqualAny

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
trait StringEditDistance extends DiffShortener:

  def editDistance(s1: String, s2: String): Int =
    foldSplittedStrings(s1, s2, 0, (r: Int, s1: String, s2: String) => {
      val matrix = new EditMatrix(s1, s2, StringLevenhsteinCosts)
      math.max(r, matrix.operations.count { case Same(_) => false; case _ => true })
    })

  /**
   * @param separators separators used to highlight differences. If sep is empty, then no delimiter is used. If sep contains
   * one character, it is taken as the unique delimiter. If sep contains 2 or more characters, the first half of the characters are taken as
   * opening delimiter and the second half as closing delimiter.
   *
   * @return a (String, String) displaying the differences between each input strings.
   * The used separators are specified by the caller. The string is shortened before and after differences if necessary. <p>
   */
  def showDistance(s1: String, s2: String, separators: String = "[]", shortenSize: Int = 20): (String, String) =
    val (r1, r2) = foldSplittedStrings(s1, s2, (Nil, Nil), (r: (List[Token], List[Token]), s1: String, s2: String) => {
      val matrix = new data.EditDistance.EditMatrix(s1, s2, StringLevenhsteinCosts)
      val operations1 = matrix.operations
      val operations2 = operations1.map(_.inverse)
      val (diffs1, diffs2) = (showDiffs(operations1, separators, shortenSize), showDiffs(operations2, separators, shortenSize))
      (skipLine(r._1) ++ diffs1, skipLine(r._2) ++ diffs2)
    })
    (r1.showTokens, r2.showTokens)

  private def skipLine(s: List[Token]): List[Token] =
    if s.isEmpty then s else s :+ Keep("\n")

  def showDiffs(operations: IndexedSeq[EditDistanceOperation[Char]], separators: String, shortenSize: Int): List[Token] =
    val delimiter = StringDelimiter(separators)

    val (isDifferent, result) = operations.foldLeft((false, Nil: List[Token])) { case ((different, res), op) =>
      if different then
        op match
          case Add(t)     => (true,  res)
          case Del(t)     => (true,  res :+ Keep(t.toString))
          case Subst(t,_) => (true,  res :+ Keep(t.toString))
          case Same(t)    => (false, res :+ Delimiter(delimiter.second) :+ Keep(t.toString))
      else
        op match
          case Add(t)     => (true,  res :+ Delimiter(delimiter.first))
          case Del(t)     => (true,  res :+ Delimiter(delimiter.first) :+ Keep(t.toString))
          case Subst(t,_) => (true,  res :+ Delimiter(delimiter.first) :+ Keep(t.toString))
          case Same(t)    => (false, res :+ Keep(t.toString))
    }
    val fullResult =
      if isDifferent then
        result :+ Delimiter(delimiter.second)
      else
        result

    shortenTokens(fullResult, Delimiter(delimiter.first), Delimiter(delimiter.second), shortenSize)

  /** apply edit distance functions on strings split on newlines so that there are no memory issues */
  def foldSplittedStrings[T](s1: String, s2: String, init: T, f: (T, String, String) => T): T =
    val (split1, split2) = split(s1, s2)
    split1.zip(split2).foldLeft(init) { (result, current) =>
      f(result, current._1, current._2)
    }

  /**
   * In order to avoid CPU and memory issues, split the 2 strings to compare:
   *  - along newlines. This is useful if the 2 strings represent a file content
   *  - otherwise split the strings so that they are less than 200 characters long
   */
  private def split(s1: String, s2: String): (List[String], List[String]) =
    val (split1, split2) = (s1.split("\n").toList, s2.split("\n").toList)
    def splitToSize(strings: List[String]) = strings.flatMap(_.splitToSize(200))
    (splitToSize(split1), splitToSize(split2))

  /**
   * Create delimiter characters
   */
  case class StringDelimiter(separators: String):
    val middle = separators.size / 2 + separators.size % 2
    val first =  if separators.isEmpty then "" else separators.substring(0, middle)
    val second = if separators.size < 2 then first else separators.substring(middle, separators.size)

/**
 * This object help shortening strings between differences when the strings are too long
 */
trait DiffShortener:

  sealed trait Token derives CanEqual
  final case class Keep(value: String) extends Token
  final case class Delimiter(value: String) extends Token
  final case class Start() extends Token
  final case class End() extends Token

  def showToken(r: Token): String =
    r match
      case Keep(c) => c
      case Delimiter(d) => d
      case Start() => ""
      case End() => ""

  extension (tokens: List[Token]) def showTokens = tokens.map(showToken).mkString

  def shorten(string: String, firstDelimiter: String = "[", secondDelimiter: String = "]", shortenSize: Int = 5): String =
    val tokens = string.map { c =>
          if c.toString == firstDelimiter then
            Delimiter(firstDelimiter)
          else if c.toString == secondDelimiter then
            Delimiter(secondDelimiter)
          else
            Keep(c.toString)
        }
    shortenTokens(tokens.toList, Delimiter(firstDelimiter), Delimiter(secondDelimiter), shortenSize).showTokens

  def shortenTokens(tokens: List[Token], firstDelimiter: Delimiter, secondDelimiter: Delimiter, shortenSize: Int = 5): List[Token] =
    def shortenLeft(ts: List[Token]): List[Token] =
      if ts.size > shortenSize
        then Keep("...") +: ts.slice(ts.size - shortenSize, ts.size)
        else ts

    def shortenRight(ts: List[Token]): List[Token] =
      if ts.size > shortenSize
        then ts.take(shortenSize) :+ Keep("...")
        else ts

    def shortenCenter(ts: List[Token]): List[Token] =
      if ts.size > shortenSize
        then (ts.take(shortenSize / 2) :+ Keep("...")) ++ ts.slice(ts.size - shortenSize / 2, ts.size)
        else ts

    val delimitedTokens = splitOnDelimiters(Start() +: tokens :+ End(), firstDelimiter, secondDelimiter)

    delimitedTokens.foldLeft(Nil: List[Token]) { (res, cur) =>
      // [abcdefgh] -> [abcdefgh]
      if cur.head == firstDelimiter && cur.last == secondDelimiter then
        res ++ cur
      // <start>abcdefgh -> ...defgh
      else if cur.headOption == Some(Start():Token) then
        res ++ shortenLeft(cur)
      // abcdefgh<end> -> abcd...
      else if cur.lastOption == Some(End():Token) then
        res ++ shortenRight(cur)
      // abcdefgh -> abc...fgh
      else
        res ++ shortenCenter(cur)
    }

  // split a list of tokens into several lists when a delimiter is found
  // abcd[efgh]ijkl[mnop]qrst -> List(abcd, [efgh], ijkl, [mnop], qrst)
  private def splitOnDelimiters(tokens: List[Token], firstDelimiter: Delimiter, secondDelimiter: Delimiter): List[List[Token]] =
    tokens.foldLeft(Nil: List[List[Token]]) { (res, cur) =>
      if cur == firstDelimiter then
        res :+ List(firstDelimiter)
      else if cur == secondDelimiter then
        res.updateLast(_ :+ secondDelimiter).toList
      else
        res.lastOption match
          case Some(ts) =>
            if ts.lastOption == Option(secondDelimiter: Token)
              then res :+ List(cur)
              else res.updateLast(_ :+ cur).toList
          case _ =>
            List(List(cur))
    }


object DiffShortener extends DiffShortener

object StringEditDistance extends StringEditDistance
