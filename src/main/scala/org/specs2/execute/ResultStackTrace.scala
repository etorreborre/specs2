package org.specs2
package execute

import control._
import main.SystemProperties
import Throwablex._

/**
 * The stacktrace for a Result
 */
private[specs2]
trait ResultStackTrace extends HasStackTrace {
  /** @return the location (file and line number) of the topmost stackTraceElement */
  def location = {
    val filtered = Throwablex.exception(stackTrace).filterNot("org.specs2").getStackTrace()
    Throwablex.exception(filtered.drop(1)).location
  }
  def exception: Throwable
}
