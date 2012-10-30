package org.specs2
package control

import io._
import specification._

class PropertySpec extends Specification with Groups { def is =
                                                                                                                        """
A Property is used to store values which can be lazily accessed when required.

It has an Option-like structure, supporting the same kind of operations and can be empty like an Option.
                                                                                                                        """^
                                                                                                                        p^
  "A property"                                                                                                          ^
    "can be empty "                                                                                                     ! g1().e1^
    "can be created from any value"                                                                                     ! g1().e2^
    "can be updated with another value"                                                                                 ! g1().e3^
    "can be updated with an option"                                                                                     ! g1().e4^
    "has a toString method returning the option value toString"                                                         ! g1().e5^
                                                                                                                        p^
  "A property can be executed"                                                                                          ^
    "and return its contained value"                                                                                    ! g2().e1^
    "it is only executed once"                                                                                          ! g2().e2^
                                                                                                                        p^
  "A property behaves like an Option"                                                                                   ^
    "with map"                                                                                                          ! g3().e1^
    "with flatMap"                                                                                                      ! g3().e2^
    "with filter"                                                                                                       ! g3().e3^
    "with foreach"                                                                                                      ! g3().e4^
    "with getOrElse"                                                                                                    ! g3().e5^
    "with isDefined"                                                                                                    ! g3().e6^
    "with isEmpty"                                                                                                      ! g3().e7^
    "with orElse"                                                                                                       ! g3().e8^
                                                                                                                        p^
  "A property can be transformed to an Either instance"                                                                 ^
    "with toLeft"                                                                                                       ! g4().e1^
    "with toRight"                                                                                                      ! g4().e2^
                                                                                                                        end

  "creation" - new g1 with prop {
    e1 := Property().isEmpty
    e2 := p1.get must_== 1
    e3 := p1.update(2).optionalValue must_== Some(2)
    e4 := p1.updateValue(Some(2)).optionalValue must_== Some(2)
    e5 := p1.toString must_== "Some(1)"
  }

  "execution"  - new g2 with prop {
    e1 := p1.toOption.get must_== 1
    e2 := {
      Property { print("one"); 1 }.toOption.get
      messages.size must_== 1
    }
  }

  "option" - new g3 with prop {
    e1 := p1.map(_.toString).get must_== "1"
    e2 := p1.flatMap(i => Some(i.toString)).get must_== "1"
    e3 := p1.filter(_ >= 0).get must_== 1
    e4 := { p1.foreach(i => print("1")); messages.size must_== 1 }
    e5 := p1.getOrElse(0) must_== 1
    e6 := p1.isDefined must beTrue
    e7 := p1.isEmpty must beFalse
    e8 := p1.orElse(Property(2)) must_== Property(1)
  }

  "either" - new g4 with prop {
    e1 := p1.toLeft(2) must_== Left(1)
    e2 := p1.toRight(2) must_== Right(1)
  }

  trait prop extends MockOutput {
    lazy val p1 = Property(1)
  }
}