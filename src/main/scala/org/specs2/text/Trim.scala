package org.specs2
package text
import scala.util.matching.Regex
import Regex.Match
import java.io.StringWriter

/**
 * Utility methods for trimming text
 */
private[specs2]
trait Trim extends control.Debug {
  /** add trimming methods to a String */
  implicit def trimmed(s: String): Trimmed = new Trimmed(s)
  /** utility conversion for StringBuffers */
  implicit def stringBufferToString(sb: java.lang.StringBuffer): Trimmed = Trimmed(sb.toString)
  /** utility conversion for StringBuffers */
  implicit def stringWriterToString(sb: StringWriter): Trimmed = Trimmed(sb.toString)

  case class Trimmed(s: String) {
    
    def trimStart(start: String) =
      if (s.trim.startsWith(start)) s.trim.drop(start.size) else s.trim
	  
    def trimEnd(end: String) =
      if (s.trim.endsWith(end)) s.trim.dropRight(end.size)  else s.trim
    
    def trimEnclosing(start: String): String = trimEnclosing(start, start)

	  def trimEnclosing(start: String, end: String): String = if (s.trim.startsWith(start) && s.trim.endsWith(end)) {
      trimStart(start).trimEnd(end).trim
    } else s
	  
	  def trimEnclosingXmlTag(t: String) = trimFirst("<"+t+".*?>").trimEnd("</"+t+">")
	  
    def removeStart(start: String) =
      if (s.startsWith(start)) s.drop(start.size) else s

    def removeEnd(end: String) =
      if (s.endsWith(end)) s.dropRight(end.size)  else s

    def removeEnclosing(toRemove: String):String = removeEnclosing(toRemove, toRemove)

	  def removeEnclosing(start: String, end: String):String =
      if (isEnclosing(start, end)) removeStart(start).removeEnd(end)
      else                                 s

	  def removeEnclosingXmlTag(t: String) =
      if (isEnclosing("<"+t, "</"+t+">")) removeFirst("<"+t+".*?>").trimEnd("</"+t+">")
      else                                s

    def isEnclosing(start: String, end: String) = s.startsWith(start) && s.endsWith(end)

    def trimNewLines = Seq("\r", "\n").foldLeft(s) { (res, cur) =>
      res.trimStart(cur).trimEnd(cur)
    }
	
    def removeNewLines = Seq("\r", "\n").foldLeft(s) { (res, cur) =>
      res.replaceAll(cur, "")
    }

    def trimFirst(exp: String) = new Regex(exp).replaceFirstIn(s.trim, "")
    def removeFirst(exp: String) = new Regex(exp).replaceFirstIn(s, "")

    def trimReplace(pairs: Pair[String, String]*) = pairs.foldLeft(s.trim) { (res, cur) =>
      res.replace(cur._1, cur._2)
    } 
    def trimReplaceAll(pairs: Pair[String, String]*) = pairs.foldLeft(s.trim) { (res, cur) =>
      res.replaceAll(cur._1, cur._2)
    } 
    def replaceAll(pairs: Pair[String, String]*) = pairs.foldLeft(s) { (res, cur) =>
      res.replaceAll(cur._1, cur._2)
    }
    def replaceInsideTag(tag: String, p: Pair[String, String]*) = {
      replaceAll(tagPattern(tag), (s: String) => java.util.regex.Matcher.quoteReplacement(s.replaceAll(p:_*)))
    }
    def replaceInsideTags(tags: String*)(p: Pair[String, String]*) = {
      tags.foldLeft(s) { (res, tag) =>
        res.replaceAll(tagPattern(tag), (s: String) => java.util.regex.Matcher.quoteReplacement(s.replaceAll(p:_*)))
      }
    }
    private def tagPattern(tag: String) = "<"+tag+">(.(.|\n)*?)</"+tag+">"

    /** replace each group with something else */
    def replaceAll(exp: String, f: String => String) = {
      new Regex(exp).replaceAllIn(s, (m: Match) => f(m.group(0)))
    }

    /** @return a sequence of lines by splitting on newlines */
    def lines: Seq[String] = s.removeAll("\r").split("\n")
    /** remove empty lines in a block of lines */
    def removeEmptyLines: String = nonEmptyLines.mkString("\n")
    /** @return split on newlines and remove empty ones */
    def nonEmptyLines: Seq[String] = Trimmed(s).lines.filterNot(_.isTrimEmpty)
    /** @return only the last block of lines when there's separated by a newline */
    def lastBlock = s.split("\n").reverse.dropWhile(_.isTrimEmpty).span(!_.isTrimEmpty)._1.reverse.mkString("\n")
    /** @return true if empty after trimming */
    def isTrimEmpty = s.trim.isEmpty

    def remove(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replace(cur, "") }
    def removeAll(remove: String) = s.replaceAll(toReplace(remove), "")

    /** split and trim each, removing empty strings */
    def splitTrim(separator: String): Seq[String] = (s.split(separator).collect { case t if !t.trim.isEmpty => t.trim }).toSeq

    private def toReplace(c: String) = c.map { letter => if ("()[]{}+-\\^$|?.*".contains(letter)) ("\\" + letter) else letter }.mkString("")
  }
}
private[specs2]
object Trim extends Trim
