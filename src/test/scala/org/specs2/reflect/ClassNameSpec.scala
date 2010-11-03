package org.specs2
package reflect

import ClassName._
import reflect._

class ClassNameSpec extends SpecificationWithJUnit { def is =

  "The class name of a fully qualified class must return only the last part" ! { 
    className("org.specs2.ClassNameSpec") must_== "ClassNameSpec" 
  }                                                                                       ^
  "The class name of an internal class should only return the last name" ! {
    class ThisClassName
    className(classOf[ThisClassName].getName) must_== "ThisClassName"
  }                                                                                       ^
  "The class name of an Int should be Integer" ! {
    className(1.asInstanceOf[Object].getClass.getName) must_== "Integer"
  }                                                                                       ^
  "The class name of a String should be String" ! {
    className("1".getClass.getName) must_== "String"
  }                                                                                       ^
                                                                                          end
}