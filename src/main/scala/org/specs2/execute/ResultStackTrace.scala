package org.specs2
package execute

import control._
import Throwablex._

/**
 * The stacktrace for a Result
 */
private[specs2]
trait ResultStackTrace extends HasStackTrace {
  /** @return the location (file and line number) of the topmost stackTraceElement */
  def location = {
    val isThrown = Throwablex.exception(stackTrace).exists("ThrownExpectations")
    val filtered = Throwablex.exception(stackTrace).filterNot("org.specs2").getStackTrace()
    if (isThrown) Throwablex.exception(filtered).location
    else          Throwablex.exception(filtered.drop(1)).location
  }
  def exception: Throwable
}
