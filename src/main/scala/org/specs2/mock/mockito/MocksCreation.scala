package org.specs2
package mock
package mockito

import control.Property
import reflect.ClassesOf
import org.mockito.invocation._
import scala.reflect.ClassTag
import org.mockito.internal.debugging.VerboseMockInvocationLogger

/**
 * This trait provides methods to create mocks and spies.
 */
trait MocksCreation extends TheMockitoMocker with ClassesOf {
  /**
   * create a mock object: val m = mock[java.util.List[String]]
   */
  def mock[T : ClassTag]: T = mocker.mock(implicitly[ClassTag[T]])
  /**
   * create a mock object with a name: val m = mockAs[java.util.List[String]]("name")
   */
  def mockAs[T : ClassTag](name: String): T = Mocked[T]().as(name)
  /**
   * create a mock object with some specific settings: val m = mock[java.util.List[String]](settings)
   */
  def mock[T : ClassTag](settings: org.mockito.MockSettings): T = Mocked[T](settings).done
  /**
   * implicit allowing to define the mock settings with a nice syntax:
	 *  - named mock: val m = mock[java.util.List[String]].as("name")
	 *  - smart mock: val m = mock[java.util.List[String]].smart
	 *  - other settings: val m = mock[java.util.List[String]].
   *	       settings(name = "list", 
   *	                defaultReturn = 10, 
	 *                  extraInterfaces = classesOf[Cloneable, Serializable])
   */
  implicit def mocked[T : ClassTag](t: =>T) = Mocked[T]()

  /** support class to create a mock object with specific settings */
  private[specs2]
  case class Mocked[T : ClassTag](mockitoSettings: org.mockito.MockSettings = org.mockito.Mockito.withSettings) {
    def as(n: String) = settings(name = n)
    def verbose = Mocked[T](mockitoSettings.invocationListeners(new VerboseMockInvocationLogger())).done
    def smart = Mocked[T](mockitoSettings.defaultAnswer(org.mockito.Mockito.RETURNS_SMART_NULLS)).done
    def defaultReturn(a: Any) = settings(defaultReturn = a)
    def defaultAnswer[S](answer: InvocationOnMock => S) = settings(defaultAnswer = (i: InvocationOnMock) => answer(i): Any)
    def extraInterface[T : ClassTag] = settings(extraInterface = implicitly[ClassTag[T]].runtimeClass)
    def extraInterfaces[T1 : ClassTag, T2: ClassTag] = settings(extraInterfaces = classesOf[T1, T2])
    def extraInterfaces[T1 : ClassTag, T2: ClassTag, T3: ClassTag] = settings(extraInterfaces = classesOf[T1, T2, T3])
    def extraInterfaces[T1 : ClassTag, T2: ClassTag, T3: ClassTag, T4: ClassTag] = settings(extraInterfaces = classesOf[T1, T2, T3, T4])

		def settings(name            : MockProperty[String] = MockProperty[String](),
		             smart           : MockProperty[Boolean] = MockProperty[Boolean](),
                 verbose         : MockProperty[Boolean] = MockProperty[Boolean](),
						     defaultAnswer   : MockProperty[InvocationOnMock => Any] = MockProperty[InvocationOnMock => Any](),
						     defaultReturn   : MockProperty[Any] = MockProperty[Any](),
						     extraInterface  : MockProperty[Class[_]] = MockProperty[Class[_]](),
		             extraInterfaces : MockProperty[Seq[Class[_]]] = MockProperty[Seq[Class[_]]]()) = {
			update(name)(n => mockitoSettings.name(n)).
			update(smart)(s => if (s) mockitoSettings.defaultAnswer(org.mockito.Mockito.RETURNS_SMART_NULLS) else mockitoSettings).
      update(verbose)(v => if (v) mockitoSettings.invocationListeners(new VerboseMockInvocationLogger()) else mockitoSettings).
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
  def smartMock[T : ClassTag]: T = Mocked[T]().smart
  /**
   * create a spy on an object. 
   * 
   * A spy is a real object but can still have some of its methods stubbed. However the syntax for stubbing a spy is a bit different than 
   * with a mock:
   * {{{
   * val s = spy(new LinkedList[String])
   * doReturn("one").when(s).get(0) // instead of s.get(0) returns "one" which would throw an exception
   * 
   * }}}
   */
  def spy[T](m: T): T = mocker.spy(m)
  /**
   * ignore stubbed methods when verifying that a mock has no more interactions
   */
  def ignoreStubs(mocks: AnyRef*): IgnoreStubs = IgnoreStubs(mocker.ignoreStubs(mocks:_*))
}

/**
 * This class encapsulate mocks which must have their stubbed methods ignored in verification methods
 */
case class IgnoreStubs(mocks: Seq[AnyRef])
