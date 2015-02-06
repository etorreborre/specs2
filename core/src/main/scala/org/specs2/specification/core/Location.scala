package org.specs2
package specification
package core

import control._

/**
 * Location of a Fragment
 *
 * This is currently implemented using stacktraces which is very brittle
 */
trait Location {
  def traceLocation(filter: StackTraceFilter): Option[TraceLocation]
  /** @return a filtered Location */
  def filter(filter: StackTraceFilter): Location
  /** file name and line number */
  def location(filter: StackTraceFilter) = traceLocation(filter).map(_.location)
  /** the class name and the line number where the Throwable was created */
  def classLocation(filter: StackTraceFilter) = traceLocation(filter).map(_.classLocation)
  /** the class name, file Name and the line number where the Throwable was created */
  def fullLocation(filter: StackTraceFilter) = traceLocation(filter).map(_.fullLocation)
  /** the line number */
  def lineNumber(filter: StackTraceFilter) = traceLocation(filter).headOption.map(_.lineNumber)

  override def toString = traceLocation(NoStackTraceFilter).fold("<empty filtered stacktrace>")(_.fullLocation)
}

case class SimpleLocation(trace: TraceLocation) extends Location {
  def traceLocation(filter: StackTraceFilter) =
    filter(Seq(trace.stackTraceElement)).map(TraceLocation.apply).headOption

  def filter(filter: StackTraceFilter) =
    traceLocation(filter).fold(this)(SimpleLocation.apply)
}

case class StacktraceLocation(trace: Seq[StackTraceElement] = (new Exception).getStackTrace) extends Location {
  def traceLocation(filter: StackTraceFilter): Option[TraceLocation] =
    filter(trace).headOption.map(TraceLocation.apply)

  /** @return a filtered Location */
  def filter(filter: StackTraceFilter) = copy(filter(trace))

  override def equals(a: Any) = a match {
    case l: StacktraceLocation => l.toString == this.toString
    case other                => false
  }

  override def hashCode =
    trace.map(_.toString).hashCode

  override def toString = s"${getClass.getSimpleName}(${traceLocation(NoStackTraceFilter)}})"
}
