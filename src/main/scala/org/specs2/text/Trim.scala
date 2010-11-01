package org.specs2
package text

/**
 * Utility methods for trimming text
 */
private[specs2]
trait Trim {
  implicit def trimmed(s: String): Trimmed = new Trimmed(s)
  class Trimmed(s: String) {
    def trimEnclosing(start: String, end: String) = {
      val result = if (s.trim.startsWith(start)) s.trim.drop(start.size) else s.trim
      if (result.endsWith(end)) result.dropRight(end.size)
      else result
    }
    def remove(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replace(cur, "") }
    def removeAll(toRemove: String*) = toRemove.foldLeft(s) { (res, cur) => res.replaceAll(cur, "") }
  }
}
private[specs2]
object Trim extends Trim
