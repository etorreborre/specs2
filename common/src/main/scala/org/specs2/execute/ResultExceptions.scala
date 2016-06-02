package org.specs2
package execute

trait ExecuteException extends Exception

/** this class allows to throw a failure result in an Exception */
case class FailureException(f: Failure) extends ExecuteException {
  override def getMessage = f.message
  override def getCause = f.exception
  override def getStackTrace = f.exception.getStackTrace
}
/** this class allows to throw a skipped result in an Exception */
case class SkipException(f: Skipped) extends ExecuteException {
  /** create a SkipException from a Failure */
  def this(f: Failure) = this(f.skip)

  override def getMessage = f.message
}

/** this class allows to throw a pending result in an Exception */
case class PendingException(f: Pending) extends ExecuteException {
  override def getMessage = f.message
}

/** this class allows to throw an Error result in an Exception */
case class ErrorException(f: Error) extends ExecuteException {
  override def getMessage = f.message
  override def getCause = f.exception
  override def getStackTrace = f.exception.getStackTrace
}

/** this class allows to throw a result that's decorated with additional information in an Exception */
case class DecoratedResultException(result: DecoratedResult[_]) extends ExecuteException
