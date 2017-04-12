package org.specs2.text

import org.specs2.specification.{Groups, script}
import org.specs2.text.NotNullStrings._

class NotNullStringsSpec extends script.Specification with Groups { def is = s2"""

 # Several functions are available to display strings without evaluation errors for
  + a null string
  + a list where the toString method is undefined
  + a map

 # It is also possible to display the class of elements in a collection
  + for an Array
  + for a Seq
  + for a Map

  the description must be extended if not all elements have the same class
   + for a Vector
   + for a Map

"""

  "toString" - new group {
    eg := (null: String).notNull must_== "null"

    eg := {
      case class L(values: Seq[Int]) extends Seq[Int] {
        override def toString = ???
        def iterator = values.iterator
        def apply(i: Int) = values(i)
        def length = values.length
      }

      L(Seq(1, 2)).notNull must_== "Exception when evaluating toString: an implementation is missing"
    }

    eg := Map(1 -> "2").notNull === "Map(1 -> 2)"

  }

  "with class" - new group {
    eg := Array(1, 2).notNullWithClass === "Array('1', '2'): Array[java.lang.Integer]"
    eg := Vector(1, 2).notNullWithClass === "Vector('1', '2'): scala.collection.immutable.Vector[java.lang.Integer]"
    eg := Map(1 -> "2", 2 -> "3").notNullWithClass === "Map('1' -> '2', '2' -> '3'): scala.collection.immutable.Map$Map2[scala.Tuple2]"

    eg := Vector(1, "2").notNullWithClass === "Vector(1: java.lang.Integer, 2: java.lang.String): scala.collection.immutable.Vector"
    eg := Map(1 -> "2", 2 -> 3).notNullWithClass === "Map(1: java.lang.Integer -> 2: java.lang.String, 2: java.lang.Integer -> 3: java.lang.Integer): scala.collection.immutable.Map$Map2"
  }
}
