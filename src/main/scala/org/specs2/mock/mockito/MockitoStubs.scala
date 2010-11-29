package org.specs2
package mock
package mockito

import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.{ OngoingStubbing, Stubber }

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
  class Stubbed [T](c: =>T) {
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
