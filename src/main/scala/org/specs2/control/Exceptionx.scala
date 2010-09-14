package org.specs2
package control

import java.io._

/**
 * This trait allows to add some utility methods to </code>Exception</code> objects.
 */
trait Exceptionx {
  /**
   * Implicit method to add additional methods to Exception objects
   */
  implicit def toExceptionx[T <: Exception](t: T) = new ExtendedException(t)  
  /**
   * See the ExtendedThrowable object description
   */
  class ExtendedException[T <: Exception](t: T) {
    private def fileName = t.getStackTrace()(0).getFileName
    private def className = t.getStackTrace()(0).getClassName.split('$')(0)
    private def lineNumber = t.getStackTrace()(0).getLineNumber

    /** @return the file name and the line number where the Throwable was created */
    def location: String = fileName + ":" + lineNumber
    /** @return the class name and the line number where the Throwable was created */
    def classLocation: String = className + ":" + lineNumber
    /** @return the class name, file Name and the line number where the Throwable was created */
    def fullLocation: String = className + " (" + location + ")"
    /** @return the stack trace as a string with where each message is separated by a new line */
    def stackToString: String = stackToString("", "\n", "\n")
    /** @return the stack trace with user-specified separators */
    def stackToString(first: String, separator: String, last: String): String = t.getStackTrace.mkString(first, separator, last)
    /** @return stack trace written using <code>Throwable.printStackTrace()</code> */
    def printStackTraceToString = {
      val w = new StringWriter
      t.printStackTrace(new PrintWriter(w))
      w.toString
    }
    /** 
     * remove all traces of this exception until the last line matching <code>name</code> is found.
     */
    def removeTracesAsFarAsNameMatches(name: String): Exception = {
      t.setStackTrace(t.getStackTrace.toList.drop(1).reverse.takeWhile { x: StackTraceElement => 
                             !x.toString.matches(".*" + name + ".*") }
                      .reverse.toArray)
      t
    }
    /** 
     * remove all traces of this exception until there's a line not matching <code>name</code>.
     */
    def removeTracesWhileNameMatches(name: String): Exception = {
      t.setStackTrace((t.getStackTrace.toList.drop(1).dropWhile { x: StackTraceElement => 
                             x.toString.matches(".*" + name + ".*") 
                          }).toArray)
      t
    }
    /**
     * throw an exception removing all the stack trace elements matching the class name of the caller.
     * @param caller object used to define the elements to remove 
     */
    def hideCallerAndThrow(caller: Object) = {
      throw removeTracesWhileNameMatches(getClassName(caller))
    }
    def hideCallerAndThrow(caller: String) = {
      throw removeTracesWhileNameMatches(caller)
    }
    /**
     * throw an exception using the stacktrace of another one.
     * @param other other exception whose stacktrace should be used 
     */
    def throwWithStackTraceOf(other: Exception) = throw t.setAs(other)
    /**
     * set an exception with the stacktrace of another one.
     * @param other other exception whose stacktrace should be used 
     */
    def setAs(other: Exception): T = {
      t.setStackTrace(other.getStackTrace)
      t.initCause(other.getCause)
      t
    }
    /**
     * return the class name of an object without $.
     */
    private def getClassName(o: Object) = o.getClass.getName.split("\\.").last.replace("$", "")
  }
}
object Exceptionx extends Exceptionx