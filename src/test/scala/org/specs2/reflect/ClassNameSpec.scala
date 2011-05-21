package org.specs2
package reflect

import mutable.Specification
import ClassName._

class ClassNameSpec extends Specification {

  "The class name of a fully qualified class must return only the last part" in { 
    simpleName(classOf[ClassNameSpec]) must_== "ClassNameSpec"
  }                                                                                       
  "The class name of an internal class should only return the last name" in {
    class ThisClassName
    simpleName(classOf[ThisClassName]) must_== "ThisClassName"
  }                                                                                       
  "The class name of an Int should be Integer" in {
    simpleName(1.asInstanceOf[Object].getClass) must_== "Integer"
  }                                                                                       
  "The class name of a String should be String" in {
    simpleName("1".getClass) must_== "String"
  }                                                                                       
  "The human name of a class should uncamel case it" in {
    humanName(classOf[ThisClass]) must_== "this class"
  }                                                                                       
  "The human name of a class should get the parent if the class is an anonymous class" in {
    humanName(anonymous.getClass) must_== "this class"
  }                                                                                       

  trait MyTrait
  class ThisClass extends MyTrait
    val anonymous = new ThisClass {
  }
}