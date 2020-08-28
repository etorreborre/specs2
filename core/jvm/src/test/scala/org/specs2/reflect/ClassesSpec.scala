package org.specs2
package reflect

import Classes._
import matcher.OperationMatchers._

class ClassesSpec extends Spec { def is = s2"""

  it is possible to instantiate a Specification
    from a class name $fromClass1
    from a class having a parameter having at least a no-args constructor that's instantiable $fromClass2
    from a nested class $fromNestedClass
    from an object name $fromObject

  if a class can not be instantiated a UserException must be created $instantiationError

"""


  def fromClass1 =
    createInstance[Specification]("org.specs2.reflect.FromClassName", getClass.getClassLoader, Nil) must beOk

  def fromClass2 =
    createInstance[Specification]("org.specs2.reflect.FromClassNameWithArg", getClass.getClassLoader, Nil) must beOk

  def fromNestedClass =
    createInstance[Specification]("org.specs2.reflect.ClassesSpec$FromNestedClass", getClass.getClassLoader, Nil) must beOk

  def fromObject =
    createInstance[Specification]("org.specs2.reflect.FromObjectName$", getClass.getClassLoader, Nil) must beOk

  def instantiationError =
    createInstance[Specification]("org.specs2.reflect.UserErrorSpecification", getClass.getClassLoader, Nil) must
      beKo("cannot create an instance for class org.specs2.reflect.UserErrorSpecification")

  class FromNestedClass extends Specification  { def is = ok }
}

class FromClassName extends Specification { def is = ok }

class System2
trait System

class UserErrorSpecification extends Specification { sys.error("boom"); def is = ok }

/** this trait cannot be instantiated directly */
trait System3 extends System

/** this class can be instantiated */
class System1(system2: System2) extends System

class FromClassNameWithArg(system: System) extends Specification:

  /** only this constructor can be instantiated */
  def this(system: System1) = this(system: System)
  def this(system: System3) = this(system: System)

  def is = ok

object FromObjectName extends Specification { def is = ok }
