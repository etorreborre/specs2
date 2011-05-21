package org.specs2
package execute

/** this class allows to throw a failure in a Exception */
case class FailureException(f: Failure) extends Exception
/** this class allows to throw a failure in a Exception */
case class SkipException(f: Skipped) extends Exception
