package org.specs2
package control

/** 
 * This trait describes something which has a stackTrace with:
 *
 *  - a list of stacktrace element
 *
 *  This is used to provide a common interface to execute.Failure and execute.Error
 */
trait HasStackTrace {
  def stackTrace: List[StackTraceElement]

  /** @return the location of the first element of the stacktrace */
  def location: String
}
