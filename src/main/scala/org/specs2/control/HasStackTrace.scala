package org.specs2
package control

/** 
 * This trait provides a stackTrace and a few utility methods on it
 */
trait HasStackTrace {
  val stackTrace: List[StackTraceElement]
  def location: String
}
