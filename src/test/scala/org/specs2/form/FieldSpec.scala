package org.specs2
package form
import execute._
import sys._
import specification.Grouped

class FieldSpec extends Specification with Grouped { def is =
                                                                                                                        """
A Field is a labelled property with can be embedded in a Form.                                                        
                                                                                                                        """^
  "A Field can be created"                                                                                              ^
    "from just a value (then its name is empty)"                                                                        ! g1.e1^
    "from a name and a value"                                                                                           ! g1.e2^
    "from existing fields, concatenating them"                                                                          ! g1.e3^
                                                                                                                        p^
  "A Field can be executed"                                                                                             ^
    "it returns skipped if the value is ok"                                                                             ! g2.e1^
    "it returns an error if the value throws an exception"                                                              ! g2.e2^
                                                                                                                        p^
  "A Field can be modified"                                                                                             ^
    "to a string Field"                                                                                                 ! g3.e1^
                                                                                                                        end

  val name     = Field("name", "eric")
  val age      = Field("age", 18)
  val ageError = Field("age", { error("error"); 18 })

  "creation" - new g1 {
    e1 := Field(18).label                     === ""
    e2 := age()                               === 18
    e3 := Field("person", name, age).toString === "person: eric/18"
  }
  "execution" - new g2 {
    e1 := age.execute      must_== skipped
    e2 := ageError.execute must be like { case Error(_, _) => ok }
  }
  "modification" - new g3 {
    e1 := age.toStringField() must_== "18"
  }
}