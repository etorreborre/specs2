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
      case f @ MatchFailure(ok, ko, _, FailureDetails(expected, actual)) => throw new ComparisonFailure(ko(), expected, actual) {
        override def getStackTrace = f.exception.getStackTrace
        override def getCause = f.exception.getCause
        override def printStackTrace = f.exception.printStackTrace
        override def printStackTrace(w: java.io.PrintStream) = f.exception.printStackTrace(w)
        override def printStackTrace(w: java.io.PrintWriter) = f.exception.printStackTrace(w)
      }
      case f @ MatchFailure(ok, ko, _, _) => throw new AssertionFailedError(ko()) {
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
