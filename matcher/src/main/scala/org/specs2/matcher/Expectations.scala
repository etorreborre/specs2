package org.specs2
package matcher

import execute._

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 */
trait Expectations extends ExpectationsCreation with TypedEqual with ExpectationsDescription

object Expectations extends Expectations
/**
 * Base trait to create expectations
 */
trait ExpectationsCreation {
  /** @return an Expectable */
  def createExpectable[T](t: =>T): Expectable[T] = createExpectable(t, None)
  /** @return an Expectable with a description */
  def createExpectable[T](t: =>T, alias: =>String): Expectable[T] = createExpectable(t, Some(Expectable.aliasDisplay(alias)))
  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: String => String): Expectable[T] = createExpectable(t, Some(alias))
  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] = Expectable(t, alias)
  /** @return an Expectable with a function to show the element T */
  def createExpectableWithShowAs[T](t: =>T, showAs: =>String): Expectable[T] = Expectable.createWithShowAs(t, showAs)

  /** this method can be overriden to throw exceptions when checking the match result */
  protected def checkFailure[T](m: MatchResult[T]): MatchResult[T] = {
    checkMatchResultFailure(mapMatchResult(setStacktrace(m)))
  }
  /** this method can be avoid filling-in a stacktrace indicating the location of the result */
  protected def setStacktrace[T](m: MatchResult[T]): MatchResult[T] = {
    m match {
      case f: MatchFailure[_] if f.trace.isEmpty => f.copy(trace = (new Exception).getStackTrace.toList)
      case other => other
    }
  }
  /** this method can be overriden to intercept a MatchResult and change its message before it is thrown */
  protected def mapMatchResult[T](m: MatchResult[T]): MatchResult[T] = m
  /** this method can be overriden to throw exceptions when checking the result */
  protected def checkResultFailure(r: =>Result): Result = r
  /** this method can be overriden to throw exceptions when checking the match result */
  protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] = m
}
