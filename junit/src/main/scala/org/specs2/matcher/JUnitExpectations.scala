package org.specs2
package matcher

import org.junit.ComparisonFailure
import junit.framework.AssertionFailedError
import execute._

/**
 * This trait throws AssertionFailed errors when a match fails.
 *
 * It is involved when reusing Matchers with JUnit
 */
trait JUnitExpectations extends ThrownExpectations {
  override protected def checkFailure[T](m: MatchResult[T]) = {
    m match {
<<<<<<< HEAD
      case f @ MatchFailure(ok, ko, _, _, NoDetails()) => throw new AssertionFailedError(f.koMessage) {
=======
      case f @ MatchFailure(ok, ko, _, FailureDetails(expected, actual)) => throw new ComparisonFailure(ko(), expected, actual) {
>>>>>>> d6fa545... collect NotImplementedErrors in AllExpectations. fixes #298
        override def getStackTrace = f.exception.getStackTrace
        override def getCause = f.exception.getCause
        override def printStackTrace = f.exception.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = f.exception.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = f.exception.printStackTrace(w)
      }
<<<<<<< HEAD
      case f @ MatchFailure(ok, ko, _, _, FailureDetails(expected, actual)) => throw new ComparisonFailure(f.koMessage, expected, actual) {
=======
      case f @ MatchFailure(ok, ko, _, _) => throw new AssertionFailedError(ko()) {
>>>>>>> d6fa545... collect NotImplementedErrors in AllExpectations. fixes #298
        override def getStackTrace = f.exception.getStackTrace
        override def getCause = f.exception.getCause
        override def printStackTrace = f.exception.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = f.exception.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = f.exception.printStackTrace(w)
      }
      case _ => ()
    }
    m
  }
}

/**
 * This trait can be imported to use MustMatchers in JUnit
 */
trait JUnitMustExpectations extends MustThrownExpectations with JUnitExpectations
object JUnitMustExpectations extends JUnitMustExpectations
/**
 * This trait can be imported to use ShouldMatchers in JUnit
 */
trait JUnitShouldExpectations extends ShouldThrownExpectations with JUnitExpectations
object JUnitShouldExpectations extends JUnitShouldExpectations
