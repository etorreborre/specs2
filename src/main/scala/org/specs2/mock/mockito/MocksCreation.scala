package org.specs2
package mock
package mockito
import control.Property
import reflect.ClassesOf
import org.mockito.stubbing._
import org.mockito.invocation._
/**
 * This trait provides methods to create mocks and spies.
 */
trait MocksCreation extends TheMockitoMocker with ClassesOf {
  /**
   * create a mock object: val m = mock[java.util.List[String]]
   */
  def mock[T : ClassManifest]: T = mocker.mock(implicitly[ClassManifest[T]])
  /**
   * create a mock object with a name: val m = mockAs[java.util.List[String]]("name")
   */
  def mockAs[T : ClassManifest](name: String): T = Mocked[T]().as(name)
  /**
   * create a mock object with some specific settings: val m = mock[java.util.List[String]](settings)
   */
  def mock[T : ClassManifest](settings: org.mockito.MockSettings): T = Mocked[T](settings).done
  /**
   * implicit allowing to define the mock settings with a nice syntax:
	 *  - named mock: val m = mock[java.util.List[String]].as("name")
	 *  - smart mock: val m = mock[java.util.List[String]].smart
	 *  - other settings: val m = mock[java.util.List[String]].
   *	       settings(name = "list", 
   *	                defaultReturn = 10, 
	 *                  extraInterfaces = classesOf[Cloneable, Serializable])
   */
  implicit def mocked[T : ClassManifest](t: =>T) = Mocked[T]()

  /** support class to create a mock object with specific settings */
  private[specs2]
  case class Mocked[T : ClassManifest](mockitoSettings: org.mockito.MockSettings = org.mockito.Mockito.withSettings) {
    def as(n: String) = settings(name = n)
    def smart = Mocked[T](mockitoSettings.defaultAnswer(org.mockito.Mockito.RETURNS_SMART_NULLS)).done 
    def defaultReturn(a: Any) = settings(defaultReturn = a)
    def defaultAnswer[S](answer: InvocationOnMock => S) = settings(defaultAnswer = (i: InvocationOnMock) => answer(i): Any)
    def extraInterface[T : ClassManifest] = settings(extraInterface = implicitly[ClassManifest[T]].erasure)

		def settings(name            : MockProperty[String] = MockProperty[String](),
		             smart           : MockProperty[Boolean] = MockProperty[Boolean](),
						     defaultAnswer   : MockProperty[InvocationOnMock => Any] = MockProperty[InvocationOnMock => Any](),
						     defaultReturn   : MockProperty[Any] = MockProperty[Any](),
						     extraInterface  : MockProperty[Class[_]] = MockProperty[Class[_]](),
		             extraInterfaces : MockProperty[Seq[Class[_]]] = MockProperty[Seq[Class[_]]]()) = {
			update(name)(n => mockitoSettings.name(n)).
			update(smart)(s => if (s) mockitoSettings.defaultAnswer(org.mockito.Mockito.RETURNS_SMART_NULLS) else mockitoSettings).
      update(defaultAnswer)(a => mockitoSettings.defaultAnswer(mocker.answer(a))).
      update(defaultReturn)(r => mockitoSettings.defaultAnswer(mocker.answer(r))).
      update(extraInterface)(i => mockitoSettings.extraInterfaces(i)).
      update(extraInterfaces)(i => mockitoSettings.extraInterfaces(i:_*)).done
		}
    /**
		 * @return the mock object
		 */
	  def done: T = mocker.mock[T](mockitoSettings)
		
		/** update the settings with a new setting value if available */
		private def update[P](prop: MockProperty[P])(f: P => org.mockito.MockSettings) = prop.toOption.map(p => Mocked[T](f(p))).getOrElse(this)
  }

  /**
   * this implicit helps with defining optional values for mockito settings
   */	
  implicit def anyToMockProperty[T](t: =>T): MockProperty[T] = MockProperty(Property(t))
  case class MockProperty[T](p: Property[T] = Property[T]()) {
    def toOption: Option[T] = p.toOption
  }
	
  /**
   * create a mock object with smart return values: val m = smartMock[java.util.List[String]]
   * 
   * This is the equivalent of Mockito.mock(List.class, SMART_NULLVALUES) but testing shows that it is not working well with Scala.
   */
  def smartMock[T : ClassManifest]: T = Mocked[T]().smart
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
  /**
   * ignore stubbed methods when verifying that a mock has no more interactions
   */
  def ignoreStubs(mocks: AnyRef*): Seq[AnyRef] = mocker.ignoreStubs(mocks:_*)
}

	