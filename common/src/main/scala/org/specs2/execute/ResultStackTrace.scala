package org.specs2
package execute

import control.*
import Throwablex.*

/** The stacktrace for a Result
  */
trait ResultStackTrace extends HasStackTrace:
  /** @return the location (file and line number) of the topmost stackTraceElement */
  def location: String =
    location(DefaultStackTraceFilter)

  def location(filter: StackTraceFilter): String =
    val filtered = Throwablex.exception(filter(stackTrace)).getStackTrace
    Throwablex.location(Throwablex.exception(filtered.toIndexedSeq))

  def exception: Throwable
