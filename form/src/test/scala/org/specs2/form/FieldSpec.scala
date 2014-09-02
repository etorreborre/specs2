package org.specs2
package form

import execute._
import sys._
import specification._
import matcher._

class FieldSpec extends script.Spec with Grouped with TypedEqual { def is = s2"""

A Field is a labelled property with can be embedded in a Form.                                                        

 # A Field can be created
   + from just a value (then its name is empty)
   + from a name and a value
   + from existing fields, concatenating them

 # A Field can be executed
   + it returns skipped if the value is ok
   + it returns an error if the value throws an exception

 # A Field can be modified
   + to a string Field
                                                                            """

  val name     = Field("name", "eric")
  val age      = Field("age", 18)
  val ageError = Field("age", { error("error"); 18 })

  "creation" - new g1 {
    e1 := Field(18).label                     === ""
    e2 := age.toOption                        === Some(18)
    e3 := Field("person", name, age).toString === "person: eric/18"
  }
  "execution" - new g2 {
    e1 := age.execute      must_== skipped
    e2 := ageError.execute must beLike { case Error(_, _) => ok }
  }
  "modification" - new g3 {
    e1 := age.toStringField.toOption must_== Some("18")
  }
}