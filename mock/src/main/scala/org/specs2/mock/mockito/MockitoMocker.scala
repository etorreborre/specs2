package org.specs2
package mock

import org.mockito.InOrder
import org.mockito.stubbing.Answer
import org.mockito.invocation._
import org.mockito.verification.VerificationMode
import org.mockito.MockSettings
import scala.reflect.ClassTag

/**
* This class is created to delegates mock methods to the Mockito static methods.
*
* @see org.specs2.mock.Mockito
*/
trait MockitoMocker {
  def verify(mode: VerificationMode) = org.mockito.Mockito.verify(org.mockito.Mockito.mock(classOf[List[Int]]), mode)
  def verifyZeroInteractions[T <: AnyRef](mocks: T*) = org.mockito.Mockito.verifyZeroInteractions(mocks:_*)
  def verifyNoMoreInteractions[T <: AnyRef](mocks: T*) = org.mockito.Mockito.verifyNoMoreInteractions(mocks:_*)

  def mock[T : ClassTag]: T = org.mockito.Mockito.mock(implicitly[ClassTag[T]].runtimeClass).asInstanceOf[T]
  def mock[T : ClassTag](settings: MockSettings): T = org.mockito.Mockito.mock(implicitly[ClassTag[T]].runtimeClass, settings).asInstanceOf[T]

  def spy[T](m: T): T = org.mockito.Mockito.spy(m)
  def when[V](v: V) = org.mockito.Mockito.when(v)
  def times(i: Int): org.mockito.internal.verification.Times = org.mockito.Mockito.times(i).asInstanceOf[org.mockito.internal.verification.Times]
  def any[T](implicit m: ClassTag[T]): T = org.mockito.Matchers.any(m.runtimeClass).asInstanceOf[T]

  def verify[M <: AnyRef](inOrder: Option[InOrder], m: M, v: VerificationMode) = {
    inOrder match {
      case Some(ordered) => ordered.verify(m, v)
      case None          => org.mockito.Mockito.verify(m, v)
    }
  }
  def verify[M](m: M, v: VerificationMode) = org.mockito.Mockito.verify(m, v)

  def doReturn[T](t: T) = org.mockito.Mockito.doReturn(t)
  def doAnswer[T](a: Answer[T]) = org.mockito.Mockito.doAnswer(a)
  def doThrow[E <: Throwable](e: E) = org.mockito.Mockito.doThrow(e)
  def doNothing = org.mockito.Mockito.doNothing

  def answer[A](a: A): Answer[A] = answer((i: InvocationOnMock) => a)
  def answer[A](f: InvocationOnMock => A): Answer[A] = new Answer[A] { def answer(i: InvocationOnMock): A = f(i) }

  def ignoreStubs(mocks: AnyRef*): Seq[AnyRef] = org.mockito.Mockito.ignoreStubs(mocks:_*).toSeq
}
