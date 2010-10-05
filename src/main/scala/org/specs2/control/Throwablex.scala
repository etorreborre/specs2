package org.specs2
package control

trait Throwablex {

  /**
   * Implicit method to add additional methods to Throwable objects
   */
  implicit def toThrowablex[T <: Throwable](t: T) = new ExtendedThrowable(t)  
  /**
   * See the ExtendedExceptions object description
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
object Throwablex extends Throwablex