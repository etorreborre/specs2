package org.specs2
package control

/** 
 * This trait describes something which has a stackTrace
 * and utility methods on it
 */
trait HasStackTrace {
  def stackTrace: List[StackTraceElement]
  def location: String
}
