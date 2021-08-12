package org.specs2
package reflect

import mutable.{Tables, Spec}
import ClassName.*
import matcher.TypedEqual

class ClassNameSpec extends Spec with Tables with TypedEqual:

  "Class names must be decoded" >> {
    "name" | "decoded" |>
      "org.specs2.name" ! "org.specs2.name" |
      "org.specs2.name$2" ! "org.specs2.name" |
      "org.specs2.name" ! "org.specs2.name" | { (name, decoded) => className(name) === decoded }
  }
  "The class name of a fully qualified class must return only the last part" >> {
    simpleName(classOf[ClassNameSpec]) === "ClassNameSpec"
  }
  "The class name of an internal class should only return the last name" >> {
    simpleName(classOf[ThisClassName]) === "ThisClassName"
  }
  "The class name of an Int should be Integer" >> {
    simpleName(1.asInstanceOf[Object].getClass) === "Integer"
  }
  "The class name of a String should be String" >> {
    simpleName("1".getClass) === "String"
  }
  "The package name of a String should be java.lang" >> {
    packageName("1".getClass.getName) === "java.lang"
  }
  "The human name of a class should uncamel case it" >> {
    humanName(classOf[ThisClass]) === "this class"
  }
  "The human name of a class should get the parent if the class is an anonymous class" >> {
    humanName(anonymous.getClass) === "this class"
  }

  trait MyTrait
  class ThisClass extends MyTrait

  val anonymous = new ThisClass {}

  class ThisClassName
