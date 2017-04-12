package org.specs2
package reflect

import specification.Grouped
import Classes._
import org.specs2.matcher.OperationMatchers._

class ClassesSpec extends Spec with Grouped { def is = s2"""

  it is possible to instantiate a Specification
    from a class name ${g1.e1}
    from a class having a parameter having at least a no-args constructor that's instantiable ${g1.e2}
    from a nested class ${g1.e3}
    from an object name ${g1.e4}

  if a class can not be instantiated a UserException must be created ${g2.e1}
                                                                                                                        """

  "instantiations" - new g1 {

    e1 := createInstance[Specification]("org.specs2.reflect.FromClassName", getClass.getClassLoader) must beOk
    e2 := createInstance[Specification]("org.specs2.reflect.FromClassNameWithArg", getClass.getClassLoader) must beOk
    e3 := createInstance[Specification]("org.specs2.reflect.ClassesSpec$FromNestedClass", getClass.getClassLoader) must beOk
    e4 := createInstance[Specification]("org.specs2.reflect.FromObjectName$", getClass.getClassLoader) must beOk
  }

  "exceptions" - new g2 {
    e1 := createInstance[Specification]("org.specs2.reflect.UserErrorSpecification", getClass.getClassLoader) must
      beKo("cannot create an instance for class org.specs2.reflect.UserErrorSpecification")
  }

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

class FromClassNameWithArg(system: System) extends Specification  {

  /** only this constructor can be instantiated */
  def this(system: System1) = this(system: System)
  def this(system: System3) = this(system: System)

  def is = ok
}

object FromObjectName extends Specification { def is = ok }
