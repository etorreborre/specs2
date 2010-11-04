package org.specs2
package mock
package mockito


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
