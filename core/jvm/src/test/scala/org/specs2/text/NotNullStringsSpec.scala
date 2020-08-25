package org.specs2
package text

import NotNullStrings._

class NotNullStringsSpec extends Specification { def is = s2"""

Several functions are available to display strings without evaluation errors for
 a null string $nullString
 a list where the toString method is undefined $noToStringDefined
 a map $aMap

It is also possible to display the class of elements in a collection
 for an Array $withClass1
 for a Seq $withClass2
 for a Map $withClass3

 the description must be extended if not all elements have the same class
  for a List $withClass4
  for a Map $withClass5

"""
  def nullString = (null: String).notNull must_== "null"

  def noToStringDefined =
    case class L(values: Seq[Int]) extends Seq[Int]:
      override def toString = ???
      def iterator = values.iterator
      def apply(i: Int) = values(i)
      def length = values.length
    L(Seq(1, 2)).notNull must_== "Exception when evaluating toString: an implementation is missing"

  def aMap = Map(1 -> "2").notNull === "Map(1 -> 2)"

  def withClass1 = Array(1, 2).notNullWithClass === "Array('1', '2'): Array[java.lang.Integer]"
  def withClass2 = Vector(1, 2).notNullWithClass === "Vector('1', '2'): scala.collection.immutable.Vector1[java.lang.Integer]"
  def withClass3 = Map(1 -> "2", 2 -> "3").notNullWithClass === "Map('1' -> '2', '2' -> '3'): scala.collection.immutable.Map$Map2[scala.Tuple2]"
  def withClass4 = Vector(1, "2").notNullWithClass === "Vector(1: java.lang.Integer, 2: java.lang.String): scala.collection.immutable.Vector1"
  def withClass5= Map(1 -> "2", 2 -> 3).notNullWithClass === "Map(1: java.lang.Integer -> 2: java.lang.String, 2: java.lang.Integer -> 3: java.lang.Integer): scala.collection.immutable.Map$Map2"
}
