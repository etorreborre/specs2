package org.specs2
package control

/** 
 * This trait describes something which has a stackTrace
 * and utility methods for it
 */
private[specs2]
trait HasStackTrace {
  def stackTrace: List[StackTraceElement]
  def location: String
}
