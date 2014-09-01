package org.specs2
package reflect

import mutable.{Tables, Spec}
import ClassName._
import matcher.TypedEqual

class ClassNameSpec extends Spec with Tables with TypedEqual {

  "Class names must be decoded" in {
    "name"                   | "decoded"                |>
    "org.specs2.name"        ! "org.specs2.name"        |
    "org.specs2.name$2"      ! "org.specs2.name"        |
    "org.specs2.name"        ! "org.specs2.name"        | { (name, decoded) => className(name) === decoded }
  }
  "The class name of a fully qualified class must return only the last part" in {
    simpleName(classOf[ClassNameSpec]) === "ClassNameSpec"
  }
  "The class name of an internal class should only return the last name" in {
    class ThisClassName
    simpleName(classOf[ThisClassName]) === "ThisClassName"
  }                                                                                       
  "The class name of an Int should be Integer" in {
    simpleName(1.asInstanceOf[Object].getClass) === "Integer"
  }                                                                                       
  "The class name of a String should be String" in {
    simpleName("1".getClass) === "String"
  }
  "The package name of a String should be java.lang" in {
    packageName("1".getClass.getName) === "java.lang"
  }
  "The human name of a class should uncamel case it" in {
    humanName(classOf[ThisClass]) === "this class"
  }                                                                                       
  "The human name of a class should get the parent if the class is an anonymous class" in {
    humanName(anonymous.getClass) === "this class"
  }                                                                                       

  trait MyTrait
  class ThisClass extends MyTrait
    val anonymous = new ThisClass {
  }
}