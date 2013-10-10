package org.specs2
package control

import io._
import specification._

class PropertySpec extends script.Specification with Groups { def is = s2"""

A Property is used to store values which can be lazily accessed when required.

It has an Option-like structure, supporting the same kind of operations and can be empty like an Option.

 ## A property
   + can be empty
   + can be created from any value
   + can be updated with another value
   + can be updated with an option
   + has a toString method returning the option value toString

 ## A property can be executed
   + and return its contained value
   + it is only executed once

 ## property behaves like an Option
   + with map
   + with flatMap
   + with filter
   + with foreach
   + with getOrElse
   + with isDefined
   + with isEmpty
   + with orElse

 ## A property can be transformed to an Either instance
   + with toLeft
   + with toRight
                                                                           """

  "creation" - new group with prop {
    eg := Property().isEmpty
    eg := p1.get must_== 1
    eg := p1.update(2).optionalValue must_== Some(2)
    eg := p1.updateValue(Some(2)).optionalValue must_== Some(2)
    eg := p1.toString must_== "Some(1)"
  }

  "execution"  - new group with prop {
    eg := p1.toOption.get must_== 1
    eg := {
      Property { print("one"); 1 }.toOption.get
      messages.size must_== 1
    }
  }

  "option" - new group with prop {
    eg := p1.map(_.toString).get must_== "1"
    eg := p1.flatMap(i => Some(i.toString)).get must_== "1"
    eg := p1.filter(_ >= 0).get must_== 1
    eg := { p1.foreach(i => print("1")); messages.size must_== 1 }
    eg := p1.getOrElse(0) must_== 1
    eg := p1.isDefined must beTrue
    eg := p1.isEmpty must beFalse
    eg := p1.orElse(Property(2)) must_== Property(1)
  }

  "either" - new group with prop {
    eg := p1.toLeft(2) must_== Left(1)
    eg := p1.toRight(2) must_== Right(1)
  }

  trait prop extends StringOutput {
    lazy val p1 = Property(1)
  }
}