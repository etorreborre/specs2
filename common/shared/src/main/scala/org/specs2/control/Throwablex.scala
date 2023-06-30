package org.specs2
package control

import java.io.*
import text.NotNullStrings.*
import text.Regexes.*

/** This trait adds some utility methods to `Throwable` objects.
  */
trait Throwablex:
  /** Extension methods for Throwables
    */
  extension [T <: Throwable](t: T)
    private def topTrace: TraceLocation =
      TraceLocation(if t.getStackTrace.isEmpty then stackTraceElement("specs2") else t.getStackTrace()(0))

  /** @return the file name and the line number where the Throwable was created */
  def location: String =
    topTrace.location

  /** @return the class name and the line number where the Throwable was created */
  def classLocation: String =
    topTrace.classLocation

  /** @return the class name, file Name and the line number where the Throwable was created */
  def fullLocation: String =
    topTrace.fullLocation

  /** @return the ith stacktrace element */
  def apply(i: Int): StackTraceElement =
    t.getStackTrace.apply(i)

  /** @return the first stacktrace element as an option */
  def headOption: Option[StackTraceElement] =
    t.getStackTrace.toList.headOption

  /** Select all traces of this exception matching a given pattern
    */
  def filter(pattern: String): T =
    t.setStackTrace(t.getStackTrace.toList.filter(patternMatches(pattern)))

  /** Select all traces of this exception according to filtering function WARNING: this mutates the exception to be able
    * to retain its type!
    */
  def filter(f: Seq[StackTraceElement] => Seq[StackTraceElement]): T =
    chainedExceptions.foreach(_.filter(f))
    t.setStackTrace(f(t.getStackTrace.toList))

  /** match a stacktrace element with a pattern */
  private def patternMatches(p: String): StackTraceElement => Boolean =
    (_: StackTraceElement).toString.matchesSafely(p, enclosing = ".*")

  /** Select all traces of this exception not matching a given pattern
    */
  def filterNot(pattern: String): T =
    t.setStackTrace(t.getStackTrace.toList.filterNot(patternMatches(pattern)))

  /** @return true if the pattern exists in one of the traces */
  def exists(pattern: String): Boolean =
    t.getStackTrace.toList.exists(patternMatches(pattern))

  /** @return the list of chained exceptions */
  def chainedExceptions: List[Throwable] =
    if t.getCause == null then List()
    else t.getCause :: t.getCause.chainedExceptions

  /** @return the list of all stacktrace elements */
  def getFullStackTrace: List[java.lang.StackTraceElement] =
    (t :: chainedExceptions).flatMap(_.getStackTrace)

  /** @return
    *   the full stack trace as a string
    */
  def getFullStackTraceAsString: String =
    val stringWriter = new java.io.StringWriter
    val pr = new PrintWriter(stringWriter)
    try { t.printStackTrace(pr) }
    finally { pr.close }
    stringWriter.toString

  /** print all the stacktrace for t, including the traces from its causes */
  def printFullStackTrace() = t.getFullStackTrace.foreach(println(_))

  /** set a new stacktrace */
  def setStackTrace(st: Seq[StackTraceElement]): T =
    t.setStackTrace(st.toArray)
    t

  /** @return the exception message and its cause if any */
  def messageAndCause =
    t.getMessage.notNull + (if t.getCause != null then ". Cause: " + t.getCause.getMessage.notNull else "")

  /** utility method to create a default stacktrace element */
  def stackTraceElement(m: String, className: String = "internals", fileName: String = "file", lineNumber: Int = 1) =
    new StackTraceElement(m, className, fileName, lineNumber)

  /** @return an exception with the given message and stacktrace */
  def exception(m: String, st: Seq[StackTraceElement], cause: Throwable = null): Exception =
    val exception = new Exception(m, cause)
    exception.setStackTrace(st.toArray)
    exception

  /** @return an exception with the given stacktrace */
  def exception(st: Seq[StackTraceElement]): Exception = exception("", st)

case class TraceLocation(path: String, fileName: String, className: String, methodName: String, lineNumber: Int):
  lazy val location: String = fileName + ":" + lineNumber

  /** the class name and the line number where the Throwable was created */
  lazy val classLocation: String = className + ":" + lineNumber

  /** the class name, file Name and the line number where the Throwable was created */
  lazy val fullLocation: String = className + " (" + location + ")"

  def stackTraceElement = new StackTraceElement(className, methodName, fileName, lineNumber)

object TraceLocation:
  def apply(t: StackTraceElement): TraceLocation =
    // path corresponding to the class name. This is an approximation corresponding to the
    // simple case of a top-level class in a file having the same name
    lazy val path = className.split("\\.").dropRight(1).mkString("", "/", "/" + fileName)
    lazy val fileName = t.getFileName
    lazy val className = t.getClassName.split('$')(0)
    lazy val lineNumber = t.getLineNumber
    TraceLocation(path, fileName, className, t.getMethodName, lineNumber)

object Throwablex extends Throwablex
