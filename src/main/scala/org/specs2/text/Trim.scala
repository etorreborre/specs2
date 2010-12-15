package org.specs2
package text
import scala.util.matching.Regex
import Regex.Match

/**
 * Utility methods for trimming text
 */
private[specs2]
trait Trim extends control.Debug {
  /** add trimming methods to a String */
  implicit def trimmed(s: String): Trimmed = new Trimmed(s)

  class Trimmed(s: String) {
    
    def trimStart(start: String) =
      if (s.trim.startsWith(start)) s.trim.drop(start.size) else s.trim
	  
    def trimEnd(end: String) =
      if (s.trim.endsWith(end)) s.trim.dropRight(end.size)  else s.trim
    
	  def trimEnclosing(start: String, end: String) = trimStart(start).trimEnd(end).trim
	  
	  def trimEnclosingXmlTag(t: String) = trimFirst("<"+t+".*?>").trimEnd("</"+t+">")
	  
    def removeStart(start: String) =
      if (s.startsWith(start)) s.drop(start.size) else s

    def removeEnd(end: String) =
      if (s.endsWith(end)) s.dropRight(end.size)  else s

	  def removeEnclosing(start: String, end: String) = removeStart(start).removeEnd(end)

	  def removeEnclosingXmlTag(t: String) = removeFirst("<"+t+".*?>").trimEnd("</"+t+">")

    def trimNewLines = Seq("\r", "\n").foldLeft(s) { (res, cur) =>
      res.trimStart(cur).trimEnd(cur)
    }
	
    def removeNewLines = Seq("\r", "\n").foldLeft(s) { (res, cur) =>
      res.removeStart(cur).removeEnd(cur)
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
      replaceAll("<"+tag+">(.(.|\n)*?)</"+tag+">", (s: String) => s.replaceAll(p:_*))
    }
    def replaceInsideTags(tags: String*)(p: Pair[String, String]*) = {
      tags.foldLeft(s) { (res, tag) =>
        res.replaceAll("<"+tag+">(.(.|\n)*?)</"+tag+">", (s: String) => s.replaceAll(p:_*))
      }
    }
    /** replace each group with something else */
    def replaceAll(exp: String, f: String => String) = {
      new Regex(exp).replaceAllIn(s, (m: Match) => f(m.group(0)))
    }
    def remove(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replace(cur, "") }
    def removeAll(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replaceAll(cur, "") }
  }
}
private[specs2]
object Trim extends Trim
