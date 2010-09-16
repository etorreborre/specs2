package org.specs2
package execute
import control._
import Exceptionx._

/**
 * This stackTrace is filtered to remove
 * the library stacktrace elements from the total stacktrace
 * unless the "-Dfullstacktrace" system property is set (for specs2 debugging purposes)
 */
trait ResultStackTrace extends HasStackTrace {
  /** @return the location (file and line number) of the topmost stackTraceElement */
  def location = exception(sanitized).location

  /** 
   * @return a filtered stacktrace, without specs2 lines (unless running a specs2 spec).
   *         Does not filter anything if the system property -Dfullstacktrace is set 
   */
  private[specs2] def sanitized = {
	if (System.getProperty("fullstacktrace") != null)
	  stackTrace
	else {
	  // filter only if the specs comes from specs2
	  val ex = 
	  if (!stackTrace.exists(_.toString matches "(org.specs2.*Spec.*|org.specs2.*Unit.*)"))
        exception(stackTrace).filterNot(".*org.specs2.*") 
      else 
        exception(stackTrace).filterNot(".*Result.*")
      ex.getStackTrace.toList
	}
  }
}