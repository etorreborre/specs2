package org.specs2
package matcher

import execute._

/**
 * This trait provides implicit definitions to transform any value into an Expectable
 */
trait Expectations extends ExpectationsCreation with TypedEqual with ExpectationsDescription

object Expectations extends Expectations

/**
 * Base trait to create expectations.
 *
 * An expectation is a value which can have an optional description and which
 * can be matched to produce a result (for example against an expected value)
 *
 * When a result is produced it can possibly be thrown as an exception
 * based on the behaviour of the ResultChecks trait
 */
trait ExpectationsCreation extends ResultChecks:

  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] =
    Expectable(() => t, Checker.pass, alias)

  /** @return an Expectable */
  def createExpectable[T](t: =>T): Expectable[T] =
    createExpectable(t, None)

  /** @return an Expectable with a description */
  def createExpectable[T](t: =>T, alias: =>String): Expectable[T] =
    createExpectable(t, Some(Expectable.aliasDisplay(alias)))

  /** @return an Expectable with a description function */
  def createExpectable[T](t: =>T, alias: String => String): Expectable[T] =
    createExpectable(t, Some(alias))

  /** @return an Expectable with a function to show the element T */
  def createExpectableWithShowAs[T](t: =>T, showAs: =>String): Expectable[T] =
    createExpectable(t).mapDescription(showAs)

  def theValue[T](t: =>T): Expectable[T] =
    createExpectable(t)

  def theBlock(t: =>Nothing): Expectable[Nothing] =
    createExpectable(t)

trait ResultChecks extends MatchResultStackTrace:

  /** this method can be overridden to throw exceptions when checking a result */
  protected def checkResultFailure(r: =>Result): Result = r

  /** this method can be overridden to throw exceptions when checking a match result */
  protected def checkMatchResultFailure[T](m: MatchResult[T]): MatchResult[T] =
    mapMatchResult(setStacktrace(m))

  /** this method can be overridden to intercept a MatchResult and modify it.
   *  It is used for example to set a stacktrace providing the location of a
   *  failure
   */
  protected def mapMatchResult[T](m: MatchResult[T]): MatchResult[T] = m

  /** @return the match result without any side-effects */
  protected def sandboxMatchResult[T](mr: =>MatchResult[T]): MatchResult[T] = mr

/** this trait allows to fill-in stack traces on match results for precise location */
trait MatchResultStackTrace:
  /** this method can be overridden to avoid filling-in a stacktrace indicating the location of the result */
  protected def setStacktrace[T](m: MatchResult[T]): MatchResult[T] =
    m match
      case f: MatchFailure[_] if f.trace.isEmpty => f.copy(trace = (new Exception).getStackTrace.toList)
      case other => other

/** this trait doesn't fill-in stack traces */
trait NoMatchResultStackTrace extends MatchResultStackTrace:
  override def setStacktrace[T](m: MatchResult[T]): MatchResult[T] = m
