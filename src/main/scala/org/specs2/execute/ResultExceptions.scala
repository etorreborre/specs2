package org.specs2
package execute

/** this class allows to throw a failure in a Exception */
case class FailureException(f: Failure) extends Exception {
  override def getMessage = f.message
  override def getCause = f.exception
  override def getStackTrace = f.exception.getStackTrace
}
/** this class allows to throw a failure in a Exception */
case class SkipException(f: Skipped) extends Exception
