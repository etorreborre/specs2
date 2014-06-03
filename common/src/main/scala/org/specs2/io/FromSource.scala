package org.specs2
package io

import control.Throwablex._
import control.TraceLocation

/**
 */
trait FromSource {
  /**
   * @return the location of the current stacktrace, possibly filtered with a function
   */
  def location(stackFilter: Seq[StackTraceElement] => Seq[StackTraceElement]): TraceLocation = {
    val stackTrace = new Exception().getStackTrace.toSeq
    val filtered = stackFilter(stackTrace)
    TraceLocation(filtered.headOption.getOrElse(stackTrace(0)))
  }

}

object FromSource extends FromSource