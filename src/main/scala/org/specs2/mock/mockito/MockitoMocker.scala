package org.specs2
package mock

import org.mockito.InOrder
import org.mockito.stubbing.Answer
import org.mockito.verification.VerificationMode

/**
 * This class is created to delegates mock methods to the Mockito static methods.
 * 
 * @see org.specs2.mock.Mockito
 */
trait MockitoMocker {
  def verify(mode: VerificationMode) = org.mockito.Mockito.verify(org.mockito.Mockito.mock(classOf[List[Int]]), mode)
  def mock[T : ClassManifest]: T = org.mockito.Mockito.mock(implicitly[ClassManifest[T]].erasure).asInstanceOf[T]
  def mock[T : ClassManifest](name: String): T = 
	  org.mockito.Mockito.mock(implicitly[ClassManifest[T]].erasure, name).asInstanceOf[T]
  def mock[T, A](implicit m: ClassManifest[T], a: org.mockito.stubbing.Answer[A]): T = 
	  org.mockito.Mockito.mock(implicitly[ClassManifest[T]].erasure, a).asInstanceOf[T]
  def smartMock[T : ClassManifest]: T = 
	  org.mockito.Mockito.mock(implicitly[ClassManifest[T]].erasure, 
	 		  org.mockito.Mockito.RETURNS_SMART_NULLS).asInstanceOf[T]
  def spy[T](m: T): T = org.mockito.Mockito.spy(m)
  def when[V](v: V) = org.mockito.Mockito.when(v)
  def times(i: Int): org.mockito.internal.verification.Times = 
	  org.mockito.Mockito.times(i).asInstanceOf[org.mockito.internal.verification.Times]
  def any[T](implicit m: ClassManifest[T]): T = org.mockito.Matchers.any(m.erasure).asInstanceOf[T]
  def verify[M <: AnyRef](inOrder: Option[InOrder], m: M, v: VerificationMode) = {
    inOrder match {
      case Some(ordered) => ordered.verify(m, v)
      case None => org.mockito.Mockito.verify(m, v)
    }
  }
  def verify[M](m: M, v: VerificationMode) = org.mockito.Mockito.verify(m, v)
  def doReturn[T](t: T) = org.mockito.Mockito.doReturn(t)
  def doAnswer[T](a: Answer[T]) = org.mockito.Mockito.doAnswer(a)
  def doThrow[E <: Throwable](e: E) = org.mockito.Mockito.doThrow(e)
  def doNothing = org.mockito.Mockito.doNothing
  def verifyNoMoreInteractions[T <: AnyRef](mocks: T*) = for (m <- mocks) org.mockito.Mockito.verifyNoMoreInteractions(m)
}
