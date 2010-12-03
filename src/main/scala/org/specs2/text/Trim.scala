package org.specs2
package text

/**
 * Utility methods for trimming text
 */
private[specs2]
trait Trim {
  /** add trimming methods to a String */
  implicit def trimmed(s: String): Trimmed = new Trimmed(s)

  class Trimmed(s: String) {
    
    def trimStart(start: String) =
      if (s.trim.startsWith(start)) s.trim.drop(start.size) else s.trim
	  
    def trimEnd(end: String) =
      if (s.trim.endsWith(end)) s.trim.dropRight(end.size)  else s.trim
    
	  def trimEnclosing(start: String, end: String) = trimStart(start).trimEnd(end).trim
	  
    def trimNewLines = s.trim.removeAll("\r", "\n")
	
    def trimReplace(pairs: Pair[String, String]*) = pairs.foldLeft(s.trim) { (res, cur) =>
      res.replace(cur._1, cur._2)
    } 
    def trimReplaceAll(pairs: Pair[String, String]*) = pairs.foldLeft(s.trim) { (res, cur) =>
      res.replaceAll(cur._1, cur._2)
    } 
    def replaceAll(pairs: Pair[String, String]*) = pairs.foldLeft(s) { (res, cur) =>
      res.replaceAll(cur._1, cur._2)
    } 
    def remove(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replace(cur, "") }
    def removeAll(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replaceAll(cur, "") }
  }
}
private[specs2]
object Trim extends Trim
