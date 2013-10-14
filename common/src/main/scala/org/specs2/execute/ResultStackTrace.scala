package org.specs2
package execute

import control._
import Throwablex._

/**
 * The stacktrace for a Result
 */
trait ResultStackTrace extends HasStackTrace {
  /** @return the location (file and line number) of the topmost stackTraceElement */
  def location = {
    val filtered = Throwablex.exception(DefaultStackTraceFilter(stackTrace)).getStackTrace
    Throwablex.exception(filtered).location
  }
  def exception: Throwable
}
