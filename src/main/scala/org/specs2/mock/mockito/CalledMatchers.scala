package org.specs2
package mock
package mockito

import control.Throwablex._
import org.mockito.verification.{ VerificationMode }
import control.Exceptions._
import matcher._
import org.mockito.InOrder
import execute.{ResultLogicalCombinators, AsResult, Result}
import ResultLogicalCombinators._

/**
 * This trait provides methods to declare expectations on mock calls:<code>
 * 
 * there was one(mockedList).get(0)
 * there was no(mockedList).get(0)
 * 
 * there was two(mockedList).get(0)
 * there was three(mockedList).get(0)
 * there was 4.times(mockedList).get(0)
 *
 * there was atLeastOne(mockedList).get(0)
 * there was atLeastTwo(mockedList).get(0)
 * there was atLeastThree(mockedList).get(0)
 * there was atLeast(4)(mockedList).get(0)
 * there was atMostOne(mockedList).get(0)
 * there was atMostTwo(mockedList).get(0)
 * there was atMostThree(mockedList).get(0)
 * there was atMost(4)(mockedList).get(0)
 * 
 * It is also possible to use a different wording:
 * 
 * there were two(mockedList).get(0)
 * got { two(mockedList).get(0) }
 * 
 * </code>
 */
trait CalledMatchers extends NumberOfTimes with FunctionArguments with TheMockitoMocker with Expectations {
  /** this matcher evaluates an expression containing mockito calls verification */
  private class CallsMatcher extends Matcher[Any] {
    def apply[S <: Any](calls: Expectable[S]) = checkCalls[S](calls)
  }

  private def checkCalls[T] = new Matcher[T] {
    def apply[S <: T](s: Expectable[S]) = {
      catchAll { s.value } { identity } match {
        case Right(v) => MatchSuccess("The mock was called as expected", "The mock was not called as expected", createExpectable(v))
        case Left(e: AssertionError) => {
          MatchFailure("The mock was called as expected",
                       "The mock was not called as expected: " + e.messageAndCause,
                       createExpectable(s.value, e.messageAndCause))
        }
        // unexpected error from inside Mockito itself
        case Left(e) => throw e
      }
    }
  }

  /** create an object supporting 'was' and 'were' methods */
  def there = new Calls
  /** 
   * class supporting 'was' and 'were' methods to forward mockito calls to the CallsMatcher matcher 
   */
  class Calls {
    def were[T](calls: =>T): MatchResult[T] = was(calls)
    def was[T](calls: =>T): MatchResult[T] = createExpectable(calls).applyMatcher(checkCalls)
  }
  /**
   * alias for 'there was'
   */
  def got[T](t: =>T) = there was t
  /**
   * implicit definition to be able to declare a number of calls 3.times(m).clear()
   */
  implicit def rangeIntToTimes(r: RangeInt): RangeIntToTimes = new RangeIntToTimes(r)
  /**
   * class providing a apply method to be able to declare a number of calls:
   *   3.times(m).clear() is actually 3.times.apply(m).clear()
   */
  class RangeIntToTimes(r: RangeInt) {
    def apply[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(r.n))(inOrder())
  }
  /** temporary InOrder object to accumulate mocks to verify in order */
//  protected implicit val mockitoHasNoInOrderOrderdingByDefault: Option[InOrder] = None
  /**
   * verify that a mock has been called appropriately
   */
  private def verify[T <: AnyRef](mock: T, v: VerificationMode)(implicit order: Option[InOrder]) = {
    order match { 
      case Some(ordered) => mocker.verify(order, mock, v)
      case None          => mocker.verify(mock, v)
    }
  }
  def inOrder(mocks: AnyRef*): Option[InOrder] =
    if (mocks.isEmpty) None
    else               Some(new org.mockito.internal.InOrderImpl(java.util.Arrays.asList(mocks:_*)))

  def inOrder(stubbed: IgnoreStubs): Option[InOrder] = inOrder(stubbed.mocks:_*)

  /** no calls made to the mock */
  def noCallsTo[T <: AnyRef](mocks: T*) = mocker.verifyZeroInteractions(mocks:_*)
  /** no call made to the mock */
  def no[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.never())(anOrder)
  /** one call only made to the mock */
  def one[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.times(1))(anOrder)
  /** two calls only made to the mock */
  def two[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.times(2))(anOrder)
  /** three calls only made to the mock */
  def three[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.times(3))(anOrder)
  /** at least n calls made to the mock */
  def atLeast[T <: AnyRef](i: Int)(mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atLeast(i))(anOrder)
  /** at least 1 call made to the mock */
  def atLeastOne[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atLeast(1))(anOrder)
  /** at least 2 calls made to the mock */
  def atLeastTwo[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atLeast(2))(anOrder)
  /** at least 3 calls made to the mock */
  def atLeastThree[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atLeast(3))(anOrder)
  /** at most n calls made to the mock */
  def atMost[T <: AnyRef](i: Int)(mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atMost(i))(anOrder)
  /** at most 1 call made to the mock */
  def atMostOne[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atMost(1))(anOrder)
  /** at most 2 calls made to the mock */
  def atMostTwo[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atMost(2))(anOrder)
  /** at most 3 calls made to the mock */
  def atMostThree[T <: AnyRef](mock: T)(implicit anOrder: Option[InOrder] = inOrder()): T = verify(mock, org.mockito.Mockito.atMost(3))(anOrder)
  /** no more calls made to the mock */
  def noMoreCallsTo[T <: AnyRef](mocks: T*): Unit = mocker.verifyNoMoreInteractions(mocks:_*)
  /** no more calls made to the mock */
  def noMoreCallsTo[T <: AnyRef](stubbed: IgnoreStubs): Unit = noMoreCallsTo(stubbed.mocks:_*)

	/** implicit def supporting calls in order */
  implicit def toInOrderMode[T : AsResult](calls: =>T): ToInOrderMode[T] = new ToInOrderMode(calls)
  /** 
   * class defining a then method to declare that calls must be made in a specific order.
   * 
   * The orderedBy method can be used to declare the mock order if there are several mocks
   */
  class ToInOrderMode[T : AsResult](calls: =>T) {
    def then[U](otherCalls: =>U): Result = {
      AsResult(calls) and createExpectable(otherCalls).applyMatcher(checkCalls).toResult
    }
  }
}

