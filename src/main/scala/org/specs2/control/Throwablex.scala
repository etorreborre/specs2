package org.specs2
package control

/**
 * Extension methods for Throwables
 */
private[specs2]
trait Throwablex {

  /**
   * implicit method to add additional methods to Throwable objects
   */
  implicit def toThrowablex[T <: Throwable](t: T) = new ExtendedThrowable(t)  
  /**
   * @see the ExtendedExceptions object description
   */
  class ExtendedThrowable[T <: Throwable](t: T) {
    /** @return the list of chained exceptions */
    def chainedExceptions: List[Throwable] = {
      if (t.getCause == null) List() 
      else t.getCause :: t.getCause.chainedExceptions
    }
    /** @return the list of all stacktrace elements */
    def getFullStackTrace: List[java.lang.StackTraceElement] = (t :: chainedExceptions).flatMap(_.getStackTrace)
  }
}
private[specs2]
object Throwablex extends Throwablex