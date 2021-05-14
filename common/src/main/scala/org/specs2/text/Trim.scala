package org.specs2
package text

import java.io.StringWriter
import java.util.regex.Pattern
import util.matching.Regex
import util.matching.Regex.Match

/**
 * Utility methods for trimming text
 */
private[specs2]
trait Trim:
  outer: Trim =>

  /** add trimming methods to a String */
  extension (s: String)

    def trimStart(start: String): String =
      if s.trim.startsWith(start) then s.trim.drop(start.length) else s.trim

    def trimEnd(end: String): String =
      if s.trim.endsWith(end) then s.trim.dropRight(end.length)  else s.trim

    def trimEndSpace: String =
      s.takeWhile(_ == ' ') + s.trim

    def trimEnclosing(start: String): String =
      trimEnclosing(start, start)

    def trimEnclosing(start: String, end: String): String =
      if s.trim.startsWith(start) && s.trim.endsWith(end) then
        trimStart(start).trimEnd(end).trim
      else s

    def trimEnclosingXmlTag(t: String): String =
      trimFirst("<"+t+".*?>").trimEnd("</"+t+">")

    def removeStart(start: String): String =
      if s.startsWith(start) then s.drop(start.length) else s

    def removeEnd(end: String): String =
      if s.endsWith(end) then s.dropRight(end.length)  else s

    def removeEnclosing(toRemove: String): String =
      removeEnclosing(toRemove, toRemove)

    def removeEnclosing(start: String, end: String): String =
      if s.isEnclosing(start, end) then
        s.removeStart(start).removeEnd(end)
      else
        s

    def removeEnclosingXmlTag(t: String): String =
      if s.isEnclosing("<"+t, "</"+t+">") then
        removeFirst("<"+t+".*?>").trimEnd("</"+t+">")
      else
        s

    def isEnclosing(start: String, end: String): Boolean =
      s.startsWith(start) && s.endsWith(end)

    def trimNewLines: String =
      Seq("\r", "\n").foldLeft(s) { (res, cur) =>
        res.trimStart(cur).trimEnd(cur)
      }

    def removeNewLines: String =
      Seq("\r", "\n").foldLeft(s) { (res, cur) =>
        res.replaceAll(cur, "")
      }

    def trimFirst(exp: String): String =
      new Regex(exp).replaceFirstIn(s.trim, "")

    def removeFirst(exp: String): String =
      new Regex(exp).replaceFirstIn(s, "")

    def removeLast(exp: String): String =
      val matches = exp.r.findAllIn(s).matchData.toSeq
      if matches.isEmpty then s
      else
        val last = matches.last
        s.substring(0, last.start) + s.substring(last.end, s.length)

    /** trim the string of everything that is before the start substring if there is one */
    def startFrom(start: String): String =
      if s.startsWith(start) || !s.contains(start) then s else new String(s.substring(s.indexOf(start)))

    def trimReplace(pairs: (String, String)*): String =
      pairs.foldLeft(s.trim) { (res, cur) =>
        res.replace(cur._1, cur._2)
      }

    def trimReplaceAll(pairs: (String, String)*): String =
      pairs.foldLeft(s.trim) { (res, cur) =>
        res.replaceAll(cur._1, cur._2)
      }

    def trimStart: String =
      s.dropWhile(Seq(' ', '\n').contains)

    def trimEnd: String =
      s.reverse.dropWhile(Seq(' ', '\n').contains).reverse

    def trimSpaceStart: String =
      s.dropWhile(Seq(' ').contains)

    def trimSpaceEnd: String =
      s.reverse.dropWhile(Seq(' ').contains).reverse

    def trimLinesSpaceEnd: String =
      s.split("\n").map(_.trimSpaceEnd).mkString("\n")

    def replaceAll(pairs: (String, String)*): String =
      pairs.foldLeft(s) { (res, cur) =>
        res.replaceAll(cur._1, cur._2)
      }

    def replaceInsideTag(tag: String, p: (String, String)*): String =
      s.replaceAll(tagPattern(tag), (s: String) => java.util.regex.Matcher.quoteReplacement(s.replaceAll(p*)))

    def replaceInsideTags(tags: String*)(p: (String, String)*): String =
      tags.foldLeft(s) { (res, tag) =>
        res.replaceAll(tagPattern(tag), (s: String) => java.util.regex.Matcher.quoteReplacement(s.replaceAll(p*)))
      }

    private def tagPattern(tag: String) = "<"+tag+">(.(.|\n)*?)</"+tag+">"

    /** replace each group with something else */
    def replaceAll(exp: String, f: String => String): String =
      new Regex(exp).replaceAllIn(s, (m: Match) => f(m.group(0).replace("\\", "\\\\")))

    /** @return a sequence of lines by splitting on newlines */
    def lines: Seq[String] =
      s.removeAll("\r").split("\n").toIndexedSeq

    /** remove empty lines in a block of lines */
    def removeEmptyLines: String = nonEmptyLines.mkString("\n")

    /** @return split on newlines and remove empty ones */
    def nonEmptyLines: Seq[String] =
      outer.lines(s).filter(l => !l.isTrimEmpty).toList

    /** @return only the last block of lines when there's separated by a newline */
    def lastBlock: String =
      s.split("\n").reverse.dropWhile(_.isTrimEmpty).span(!_.isTrimEmpty)._1.reverse.mkString("\n")

    /** @return true if empty after trimming */
    def isTrimEmpty: Boolean = s.trim.isEmpty

    def remove(toRemove: String*): String =
      toRemove.foldLeft(s) { (res, cur) => res.replace(cur, "") }

    def removeAll(remove: String): String =
      s.replaceAll(Pattern.quote(remove), "")

    /** split and trim each, removing empty strings */
    def splitTrim(separator: String): Seq[String] =
      s.split(separator).collect { case t if !t.trim.isEmpty => t.trim}.toSeq

    /** truncate a string to a given number of characters and ellide the missing characters with ... */
    def truncate(length: Int): String =
      if s.length > length then s.take(length - 3)+"..."
      else s

  extension (s: String)
    def offset(n: Int): String =
      if n == 0 then
        s
      else
        s.split("\n", -1).map(l => offsetLine(l, n)).mkString("\n")

  private def offsetLine(l: String, n: Int) =
    if n > 0  then
      " " * n + l
    else
      l.takeWhile(_ == ' ').drop(-n).mkString + l.dropWhile(_ == ' ').mkString

private[specs2]
object Trim extends Trim
