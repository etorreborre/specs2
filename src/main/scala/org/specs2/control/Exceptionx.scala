package org.specs2
package control

/**
 * This trait allows to add some utility methods to </code>Exception</code> objects.
 */
private [specs2]
trait Exceptionx {
  /**
   * Implicit method to add additional methods to Exception objects
   */
  implicit def extend[T <: Exception](t: T) = new ExtendedException(t)  
  /**
   * See the ExtendedExceptions object description
   */
  class ExtendedException[T <: Exception](t: T) {
    private val topTrace = new Location(if (t.getStackTrace().isEmpty) stackTraceElement("specs2") else t.getStackTrace()(0)) 
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
      t.setStackTrace(t.getStackTrace.toList.filter(_.toString matches (".*"+pattern+".*")).toArray)
      t
    }
    /** 
     * Select all traces of this exception not matching a given pattern
     */
    def filterNot(pattern: String) = {
      t.setStackTrace(t.getStackTrace.toList.filterNot(_.toString matches (".*"+pattern+".*")).toArray)
      t
    }
  }
  /** utility method to create a default stacktrace element */
  def stackTraceElement(m: String, className: String = "internals", fileName: String = "file", lineNumber: Int = 1) = 
	   new StackTraceElement(m, className, fileName, lineNumber)
  /** @return an exception with the given message and stacktrace */
  def exception(m: String, st: List[StackTraceElement]): Exception = {
	  val exception = new Exception(m)
	  exception.setStackTrace(st.toArray)
	  exception
  }
  /** @return an exception with the given stacktrace */
  def exception(st: List[StackTraceElement]): Exception = exception("", st)
  /** location information from a stackTrace element */
  class Location(t: StackTraceElement) {
    /** path corresponding to the class name. This is an approximation corresponding to the
     *  simple case of a top-level class in a file having the same name */
    lazy val path = className.replace(".", "/")+".scala"
    lazy val fileName = t.getFileName
    lazy val className = t.getClassName.split('$')(0)
    lazy val lineNumber = t.getLineNumber
    lazy val location: String = fileName + ":" + lineNumber
    /** the class name and the line number where the Throwable was created */
    lazy val classLocation: String = className + ":" + lineNumber
    /** the class name, file Name and the line number where the Throwable was created */
    lazy val fullLocation: String = className + " (" + location + ")"
  }
}
private [specs2]
object Exceptionx extends Exceptionx