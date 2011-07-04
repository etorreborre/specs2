package org.specs2
package control

import java.io. { StringWriter, PrintWriter }

/**
 * This trait allows to add some utility methods to `Throwable` objects.
 */
private [specs2]
trait Throwablex {
  /**
   * Implicit method to add additional methods to Throwable objects
   */
  implicit def extend[T <: Throwable](t: T) = new ExtendedThrowable(t)  
  /**
   * See the ExtendedExceptions object description
   */
  class ExtendedThrowable[T <: Throwable](t: T) {
    private val topTrace = new TraceLocation(if (t.getStackTrace().isEmpty) stackTraceElement("specs2") else t.getStackTrace()(0))
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
     * Select all traces of this exception matching a given pattern
     */
    def filter(pattern: String) = {
      setStackTrace(t.getStackTrace.toList.filter(_.toString matches (".*"+pattern+".*")))
    }
    /**
     * Select all traces of this exception not matching a given pattern
     */
    def filterNot(pattern: String) = {
      setStackTrace(t.getStackTrace.toList.filterNot(_.toString matches (".*"+pattern+".*")))
    }
    /** @return the list of chained exceptions */
    def chainedExceptions: List[Throwable] = {
      if (t.getCause == null) List() 
      else t.getCause :: t.getCause.chainedExceptions
    }
    /** @return the list of all stacktrace elements */
    def getFullStackTrace: List[java.lang.StackTraceElement] = (t :: chainedExceptions).flatMap(_.getStackTrace)
    /**
     * @return the full stack trace as a string
     */
    def getFullStackTraceAsString: String = {
       val stringWriter = new java.io.StringWriter
       val pr = new PrintWriter(stringWriter)
       try { t.printStackTrace(pr) } finally { pr.close }
       stringWriter.toString
    } 

    /** print all the stacktrace for t, including the traces from its causes */
    def printFullStackTrace = t.getFullStackTrace.foreach(println(_))

    /** set a new stacktrace */
    private def setStackTrace(st: Seq[StackTraceElement]) = {
      t.setStackTrace(st.toArray)
      t
    }

  }
  /** utility method to create a default stacktrace element */
  def stackTraceElement(m: String, className: String = "internals", fileName: String = "file", lineNumber: Int = 1) = 
	   new StackTraceElement(m, className, fileName, lineNumber)
  /** @return an exception with the given message and stacktrace */
  def exception(m: String, st: Seq[StackTraceElement]): Exception = {
	  val exception = new Exception(m)
	  exception.setStackTrace(st.toArray)
	  exception
  }
  /** @return an exception with the given stacktrace */
  def exception(st: Seq[StackTraceElement]): Exception = exception("", st)
  /** location information from a stackTrace element */
}

private[specs2]
class TraceLocation(t: StackTraceElement) {
  /** path corresponding to the class name. This is an approximation corresponding to the
   *  simple case of a top-level class in a file having the same name */
  lazy val path = className.split("\\.").dropRight(1).mkString("", "/", "/"+fileName)
  lazy val fileName = t.getFileName
  lazy val className = t.getClassName.split('$')(0)
  lazy val lineNumber = t.getLineNumber
  lazy val location: String = fileName + ":" + lineNumber
  /** the class name and the line number where the Throwable was created */
  lazy val classLocation: String = className + ":" + lineNumber
  /** the class name, file Name and the line number where the Throwable was created */
  lazy val fullLocation: String = className + " (" + location + ")"
}

private [specs2]
object Throwablex extends Throwablex