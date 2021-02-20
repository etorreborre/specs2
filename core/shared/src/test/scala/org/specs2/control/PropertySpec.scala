package org.specs2
package control

import io.*

class PropertySpec extends Spec { def is = s2"""

A Property is used to store values which can be lazily accessed when required.

It has an Option-like structure, supporting the same kind of operations and can be empty like an Option.

A property
 can be empty $property1
 can be created from any value $property2
 can be updated with another value $property3
 can be updated with an option $property4
 has a toString method returning the option value toString $property5

A property can be executed
 and return its contained value $execution1
 it is only executed once $execution2

A property behaves like an Option
 with map $option1
 with flatMap $option2
 with filter $option3
 with foreach $option4
 with getOrElse $option5
 with isDefined $option6
 with isEmpty $option7
 with orElse $option8

A property can be transformed to an Either instance
 with toLeft $toEither1
 with toRight $toEither2

"""

 def property1 = Property().isEmpty
 def property2 = { val p = prop(); p.p1.toOption `must` ===(Some(1)) }
 def property3 = { val p = prop(); p.p1.update(2).toOption `must` ===(Some(2)) }
 def property5 = { val p = prop(); p.p1.toString `must` ===("1") }
 def property4 = { val p = prop(); p.p1.updateValue(Some(2)).toOption `must` ===(Some(2)) }

 def execution1 = { val p = prop(); import p.*;
  p1.toOption.get `must` ===(1)
}

 def execution2 =
   val p = prop(); import p.*
   Property { print("one"); 1 }.toOption.get
   messages.size `must` ===(1)

 def option1 = { val p = prop(); p.p1.map(_.toString).toOption `must` ===(Some("1")) }
 def option2 = { val p = prop(); p.p1.flatMap(i => Some(i.toString)).toOption `must` ===(Some("1")) }
 def option3 = { val p = prop(); p.p1.filter(_ >= 0).toOption `must` ===(Some(1)) }
 def option4 = { val p = prop(); p.p1.foreach(i => p.print("1")); p.messages.size `must` ===(1) }
 def option5 = { val p = prop(); p.p1.getOrElse(0) `must` ===(1) }
 def option6 = { val p = prop(); p.p1.isDefined `must` beTrue }
 def option7 = { val p = prop(); p.p1.isEmpty `must` beFalse }
 def option8 = { val p = prop(); p.p1.orElse(Property(2)) `must` ===(Property(1)) }

 def toEither1 = { val p = prop(); p.p1.toLeft(2) `must` ===(Left(1)) }
 def toEither2 = { val p = prop(); p.p1.toRight(2) `must` ===(Right(1)) }

  case class prop() extends StringOutput:
    lazy val p1 = Property(1)
}
