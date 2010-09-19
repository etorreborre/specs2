package org.specs2
package mock
import matcher._
import control.Exceptions._
import control.LocalVariables._
import org.mockito.InOrder
import org.mockito.stubbing.Answer
import org.mockito.internal.stubbing.StubberImpl
import org.mockito.invocation.InvocationOnMock
import org.mockito.internal.InOrderImpl 
import org.mockito.verification.{ VerificationMode }
import org.mockito.internal.stubbing._
import org.mockito.stubbing.{ OngoingStubbing, Stubber }


trait Mockito extends MocksCreation with CalledMatchers with MockitoStubs 
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
trait CalledMatchers extends NumberOfTimes with TheMockitoMocker with Expectations {
  /** this matcher evaluates an expression containing mockito calls verification */
  private class CallsMatcher extends Matcher[Any] {
    def apply[S <: Any : Expectable](calls: =>S) = checkCalls[S](calls, implicitly[Expectable[S]])
  }
  private def checkCalls[T](calls: =>T, expectable: Expectable[T]): MatchResult[T] = {
    catchAll { calls } { identity } match {
   	  case Right(v) => new MatchSuccess("The mock was called as expected", "The mock was not called as expected", new Expectable(v))
  	  case Left(e) => 
   	    new MatchFailure("The mock was called as expected", 
  			             "The mock was not called as expected: " + e.getMessage, 
  			             new Expectable(calls) { override def description = e.getMessage })
    }
  }
  /** create an object supporting 'was' and 'were' methods */
  def there = new Calls
  /** 
   * class supporting 'was' and 'were' methods to forward mockito calls to the CallsMatcher matcher 
   */
  class Calls {
    def were[T](calls: =>T): MatchResult[T] = was(calls)
    def was[T](calls: =>T): MatchResult[T] = checkCalls(calls, new Expectable(calls))
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
    def apply[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(r.n))
  }
  /** temporary InOrder object to accumulate mocks to verify in order */
  protected implicit val mockitoHasNoInOrderOrderdingByDefault: Option[InOrderImpl] = None
  /**
   * verify that a mock has been called appropriately
   * if an inOrder object has been previously created (which means we're verifying the mocks calls order),
   * then the mock is added to the inOrder object and the inOrder object is used for the verification.
   * 
   * Otherwise a normal verification is performed
   */
  private def verify[T <: AnyRef](mock: =>T, v: VerificationMode)(implicit order: Option[InOrderImpl]) = {
    order match { 
      case Some(ordered) => { 
        val mocksList = ordered.getMocksToBeVerifiedInOrder()
        if (!mocksList.contains(mock))
          mocksList.add(mock)
        mocker.verify(Some(new InOrderImpl(mocksList)), mock, v)
      }
      case None => mocker.verify(mock, v)
    }
  }
  def inOrder(mocks: AnyRef*) = {
	val order = new InOrderImpl(new java.util.ArrayList[Object])
    val mocksList = order.getMocksToBeVerifiedInOrder()
    mocks foreach { m => mocksList.add(m) }
    Some(new InOrderImpl(mocksList))
  }
  /** no call made to the mock */
  def no[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.never())
  /** one call only made to the mock */
  def one[T <: AnyRef](mock: =>T)(implicit anOrder: Option[InOrderImpl]) = verify(mock, org.mockito.Mockito.times(1))(anOrder)
  /** two calls only made to the mock */
  def two[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(2))
  /** three calls only made to the mock */
  def three[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.times(3))
  /** at least n calls made to the mock */
  def atLeast[T <: AnyRef](i: Int)(mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(i))
  /** at least 1 call made to the mock */
  def atLeastOne[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(1))
  /** at least 2 calls made to the mock */
  def atLeastTwo[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(2))
  /** at least 3 calls made to the mock */
  def atLeastThree[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atLeast(3))
  /** at most n calls made to the mock */
  def atMost[T <: AnyRef](i: Int)(mock: =>T) = verify(mock, org.mockito.Mockito.atMost(i))
  /** at most 1 call made to the mock */
  def atMostOne[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(1))
  /** at most 2 calls made to the mock */
  def atMostTwo[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(2))
  /** at most 3 calls made to the mock */
  def atMostThree[T <: AnyRef](mock: =>T) = verify(mock, org.mockito.Mockito.atMost(3))
  /** no more calls made to the mock */
  def noMoreCallsTo[T <: AnyRef](mock: =>T) = mocker.verifyNoMoreInteractions(mock)
  /** implicit def supporting calls in order */
  implicit def toInOrderMode[T](calls: =>T): ToInOrderMode[T] = new ToInOrderMode(calls)
  /** 
   * class defining a then method to declare that calls must be made in a specific order.
   * 
   * The orderedBy method can be used to declare the mock order if there are several mocks
   */
  class ToInOrderMode[T](calls: =>T) {
    def then[U](otherCalls: =>U) = {
      val f = () => {
    	calls
        otherCalls 
      }
      new Expectable(f()).applyMatcher(new CallsMatcher)
    }
  }
}
/**
 * This trait provides functionalities to declare stub values on method calls.
 * 
 * Usage:<code>
 * 
 * mockedList.get(0) returns "one"
 * mockedList.get(0) returns ("one", "two")
 * mockedList.get(0) throws new Exception("unexpected")
 * mockedList.get(0) answers ( i => "value " + i.toString )
 * 
 * </code>
 * 
 * It is also possible to chain stubs like this: <code>
 * 
 * mockedList.get(0) returns "one" thenReturns "two"
 * mockedList.get(0) returns "one" thenThrows new Exception("unexpected now")
 * </code>
 */
trait MockitoStubs extends MocksCreation {
  /** delegate to MockitoMocker doAnswer with a MockAnswer object using the function f. */
  def doAnswer[T](f: Any => T) = mocker.doAnswer(new MockAnswer(f))
  
  /** @return an object supporting the stub methods. */
  implicit def theStubbed[T](c: =>T) = new Stubbed(c)

  /** 
   * This class provide stub methods like returns, throws and answers.
   * Internally it calls Mockito.when(mock call).thenReturn(returnValue)
   */
  class Stubbed	[T](c: =>T) {
    def returns(t: T, t2: T*): OngoingStubbing[T] = {
      if (t2.isEmpty) 
        mocker.when(c).thenReturn(t)
      else
    	t2.foldLeft (mocker.when(c).thenReturn(t)) { (res, cur) => res.thenReturn(cur) }
    }
    def answers(function: Any => T) = mocker.when(c).thenAnswer(new MockAnswer(function))
    def throws[E <: Throwable](e: E*): OngoingStubbing[T] = {
      if (e.isEmpty) throw new java.lang.IllegalArgumentException("The parameter passed to throws must not be empty")
      e.drop(1).foldLeft(mocker.when(c).thenThrow(e.head)) { (res, cur) => res.thenThrow(cur) }
    }
  }
  /** @return an object allowing the chaining of returned values on doNothing calls. */
  implicit def aStubber(stub: =>Stubber) = new AStubber(stub)
  /** provide stub chain methods. */
  class AStubber[T](stub: =>Stubber) {
    def thenReturn[T](t: T) = stub.doReturn(t)
    def thenThrow[E <: Throwable](e: E) = stub.doThrow(e)
  }
  /** @return an object allowing the chaining of stub values. */
  implicit def anOngoingStubbing[T](stub: =>OngoingStubbing[T]): AnOngoingStubbing[T] = new AnOngoingStubbing(stub)
  /** provide stub chain methods. */
  class AnOngoingStubbing[T](stub: =>OngoingStubbing[T]) {
    def thenReturns(t: T) = stub.thenReturn(t)
    def thenThrows[E <: Throwable](e: E) = stub.thenThrow(e)
  }
  /** allows to use a specs matcher to match parameters by encapsulating it as a Hamcrest matcher. */
  implicit def argThat[T](m: org.specs2.matcher.Matcher[T]): T = org.mockito.Matchers.argThat(new org.specs2.mock.HamcrestMatcherAdapter(m))
  /** allows to use a hamcrest matchers to match parameters. */
  def argThat[T](m: org.hamcrest.Matcher[T]): T = org.mockito.Matchers.argThat(m)

  /** 
   * This class is an implementation of the Answer interface allowing to pass functions as an answer.
   * 
   * It does a bit of work for the client:
   * 
   * // if the method has one parameter and the function also, the parameter is passed
   * mock.get(0) answers ( i => i.toString )
   * 
   * // if the method has one parameter and the function has two, the mock is passed as the second argument
   * mock.get(0) answers { (i, mock) => i.toString + " for mock " + mock.toString } 
   * 
   * Similarly a mocked method with no parameters can use a function with one parameter. In that case, the mock will be passed
   * mock.size answers { mock => mock.hashCode } 
   * 
   * In any other cases, if f is a function of 1 parameter, the array of the method parameters will be passed and if the function has
   * 2 parameters, the second one will be the mock.
   * 
   */
  class MockAnswer[T](function: Any => T) extends Answer[T] {
     def answer(invocation: InvocationOnMock): T = {
       val args = invocation.getArguments
       val mock = invocation.getMock
       if (args.size == 0) {
         function match {
           case f: Function0[_] => return f()
           case f: Function1[_,_] => return f(mock)
         }
       } else if (args.size == 1) {
         function match {
           case f: Function1[_, _] => return f(args(0))
         }
         function match {
           case f2: Function2[_, _, _] => return f2(args(0), mock)
         }
       } else {
         function match {
           case f: Function1[_, _] => return f(args)
         }
         function match {
           case f2: Function2[_, _, _] => return f2(args, mock)
         }
       }
     } 
  }
}

trait NumberOfTimes {
  /** 
   * This implicit definition allows to declare a number of times
   * <code>3.times</code>
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) { 
    def times = this 
  }
}

/**
 * This trait provides methods to create mocks and spies.
 */
trait MocksCreation extends TheMockitoMocker {
  /**
   * create a mock object: val m = mock[java.util.List[String]]
   */
  def mock[T : ClassManifest]: T = mocker.mock(implicitly[ClassManifest[T]])
  /**
   * create a mock object with a name: val m = mockAs[java.util.List[String]]("name")
   */
  def mockAs[T : ClassManifest](name: String): T = mocker.mock(name)
  /**
   * implicit allowing the following syntax for a named mock: val m = mock[java.util.List[String]],as("name")
   */
  implicit def mockToAs[T : ClassManifest](t: =>T) = new NamedMock(t)
  
  /** support class to create a mock object with a name */
  class NamedMock[T : ClassManifest](t: =>T) {
    def as(name: String): T = mockAs[T](name)
  }

  /**
   * create a mock object with smart return values: val m = smartMock[java.util.List[String]]
   * 
   * This is the equivalent of Mockito.mock(List.class, SMART_NULLVALUES) but testing shows that it is not working well with Scala.
   */
  def smartMock[T : ClassManifest]: T = mocker.smartMock
  /**
   * create a spy on an object. 
   * 
   * A spy is a real object but can still have some of its methods stubbed. However the syntax for stubbing a spy is a bit different than 
   * with a mock:<code>
   * 
   * val s = spy(new LinkedList[String])
   * doReturn("one").when(s).get(0) // instead of s.get(0) returns "one" which would throw an exception
   * 
   * </code>
   */
  def spy[T](m: T): T = mocker.spy(m)
}

/**
 * shortcuts to standard Mockito functions
 */
trait MockitoFunctions extends TheMockitoMocker {
    /** delegate to MockitoMocker doReturn. */
  def doReturn[T](t: T) = mocker.doReturn(t)
  /** delegate to MockitoMocker doAnswer. */
  def doAnswer[T](a: Answer[T]) = mocker.doAnswer(a)
  /** delegate to MockitoMocker doThrow. */
  def doThrow[E <: Throwable](e: E) = mocker.doThrow(e)
  /** delegate to MockitoMocker doNothing. */
  def doNothing = mocker.doNothing
}

/** delegate to Mockito static methods with appropriate type inference. */
trait TheMockitoMocker {
  private[specs2] val mocker = new MockitoMocker {}	
}