package org.specs2
package control

import java.io._

/**
 * This trait allows to add some utility methods to </code>Exception</code> objects.
 */
private [specs2]
trait Exceptionx {
  /**
   * Implicit method to add additional methods to Exception objects
   */
  implicit def toExceptionx[T <: Exception](t: T) = new ExtendedException(t)  
  /**
   * See the ExtendedThrowable object description
   */
  class ExtendedException[T <: Exception](t: T) {
    private val topTrace = new Location(if (t.getStackTrace().isEmpty) new StackTraceElement("specs2", "internals", "file", 1) else t.getStackTrace()(0)) 
    /** @return the file name and the line number where the Throwable was created */
    def location = topTrace.location
    /** @return the class name and the line number where the Throwable was created */
    def classLocation = topTrace.classLocation
    /** @return the class name, file Name and the line number where the Throwable was created */
    def fullLocation= topTrace.fullLocation
    /** @return the ith stacktrace element */
    def apply(i: Int) = t.getStackTrace()(i)
    /** @return the first stacktrace element as an option */
    def headOption = t.getStackTrace().toList.headOption
    /** 
     * filter all traces of this exception matching a given pattern
     */
    def filter(pattern: String) = {
      t.setStackTrace(t.getStackTrace.toList.filter(_.toString matches (".*"+pattern+".*")).toArray)
      t
    } 
    /** 
     * filter all traces of this exception not matching a given pattern
     */
    def filterNot(pattern: String) = {
      t.setStackTrace(t.getStackTrace.toList.filterNot(_.toString matches (".*"+pattern+".*")).toArray)
      t
    }
    def updateMessage(m: String) = {
      t
    }
  }
  class Location(t: StackTraceElement) {
    val fileName = t.getFileName
    val className = t.getClassName.split('$')(0)
    val lineNumber = t.getLineNumber
    def location: String = fileName + ":" + lineNumber
    /** @return the class name and the line number where the Throwable was created */
    def classLocation: String = className + ":" + lineNumber
    /** @return the class name, file Name and the line number where the Throwable was created */
    def fullLocation: String = className + " (" + location + ")"
  }
}
object Exceptionx extends Exceptionx