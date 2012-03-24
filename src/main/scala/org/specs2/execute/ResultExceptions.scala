package org.specs2
package execute

/** this class allows to throw a failure result in an Exception */
case class FailureException(f: Failure) extends Exception {
  override def getMessage = f.message
  override def getCause = f.exception
  override def getStackTrace = f.exception.getStackTrace
}
/** this class allows to throw a skipped result in an Exception */
case class SkipException(f: Skipped) extends Exception {
  /** create a SkipException from a Failure */
  def this(f: Failure) = this(f.skip)
}

/** this class allows to throw a pending result in an Exception */
case class PendingException(f: Pending) extends Exception

