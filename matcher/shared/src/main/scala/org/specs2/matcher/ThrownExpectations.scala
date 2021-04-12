package org.specs2
package matcher

import execute.*
import execute.Skipped
import execute.Pending
import execute.Failure
import execute.PendingException
import execute.SkipException
import execute.FailureException

/**
 * Thrown expectations will throw a FailureException if a match fails
 *
 * This trait can be extended to be used in another framework like ScalaTest:
 *
 *   trait ScalaTestExpectations extends ThrownExpectations {
 *     override protected def checkResultFailure(r: =>Result) = {
 *       r match {
 *         case f @ Failure(ko, _, _, _) => throw new TestFailedException(f.message, f.exception, 0)
 *         case _ => ()
 *       }
 *       m
 *     }
 *   }
 */
trait ThrownExpectations extends ThrownExpectationsCreation with ThrownStandardResults with ThrownStandardMatchResults

/**
 * Lightweight ThrownExpectations trait with less implicit methods
 */
trait ThrownExpectationsCreation extends ThrownExpectables

trait ThrownExpectables extends ExpectationsCreation:

  /** @return an Expectable with a description function */
  override def createExpectable[T](t: =>T, alias: Option[String => String]): Expectable[T] =
    val checker = new Checker:
      def check[T](result: Result): Result =
        checkResultFailure(result)

    Expectable(() => t, checker, alias)

  override protected def checkResultFailure(result: =>Result) =
    lazy val r = result
    r match
      case f@Failure(_, _, _, _) => throw new FailureException(f)
      case s@Skipped(_, _) => throw new SkipException(s)
      case s@Pending(_) => throw new PendingException(s)
      case e@Error(_, _) => throw new ErrorException(e)
      case d@DecoratedResult(_, r) => if !r.isSuccess then throw new DecoratedResultException(d) else ()
      case _ => ()
    r

trait ThrownStandardResults extends StandardResults with ExpectationsCreation:
  override def failure: Failure = { checkResultFailure(throw new FailureException(StandardResults.failure)); StandardResults.failure }
  override def todo: Pending = { checkResultFailure(throw new PendingException(super.todo)); super.todo }
  override def anError: Error = { checkResultFailure(throw new ErrorException(super.anError)); super.anError }

  override lazy val success = Success("success")

  override def success(m: String): Success = { checkResultFailure(Success(m)); Success(m) }
  override def failure(m: String): Failure = failure(Failure(m))

  protected def failure(f: Failure): Failure = { checkResultFailure(throw new FailureException(f)); f }

  override def pending: Pending = pending("PENDING")
  override def pending(m: String): Pending = pending(Pending(m))
  protected def pending(p: Pending): Pending = { checkResultFailure(throw new PendingException(p)); p }

  override def skipped: Skipped = skipped("skipped")
  override def skipped(m: String): Skipped = skipped(Skipped(m))
  protected def skipped(s: Skipped): Skipped = { checkResultFailure(throw new SkipException(s)); s }

trait ThrownStandardMatchResults extends ExpectedResults with ExpectationsCreation:
  override lazy val ko: Result =
    checkResultFailure(throw new FailureException(Failure("ko")))

  /** @return the value without any side-effects for expectations */
  override def sandboxResult(r: =>Result): Result =
    try r
    catch
      case FailureException(e) => e.asInstanceOf[Result]
      case SkipException(e)    => e.asInstanceOf[Result]
      case PendingException(e) => e.asInstanceOf[Result]
      case other: Throwable    => throw other


object ThrownExpectations extends ThrownExpectations

/**
 * This trait can be used to cancel the effect of thrown expectations.
 *
 * For example it can be mixed-in a mutable.Specification so that no exception is thrown on failure
 */
trait NoThrownExpectations extends Expectations:
  override protected def checkResultFailure(r: =>Result) = r

/**
 * This trait can be used to integrate failures and skip messages into specs2
 */
trait ThrownMessages:
  this: ThrownExpectations =>

  def fail(m: String): Nothing = throw new FailureException(Failure(m))
  def skip(m: String): Nothing = throw new SkipException(Skipped(m))
