package org.specs2
package form
import execute._

class FieldSpec extends SpecificationWithJUnit { def is =
                                                                                                                        """
A Field is a labelled property with can be embedded in a Form.                                                        
                                                                                                                        """^
  "A Field can be created"                                                                                              ^
    "from just a value (then its name is empty)"                                                                        ! creation.e1^
    "from a name and a value"                                                                                           ! creation.e2^
    "from existing fields, concatenating them"                                                                          ! creation.e3^
                                                                                                                        p^
  "A Field can be executed"                                                                                             ^
    "it returns skipped if the value is ok"                                                                             ! execute.e1^
    "it returns an error if the value throws an exception"                                                              ! execute.e2^
                                                                                                                        p^
  "A Field can be modified"                                                                                             ^
    "to a string Field"                                                                                                 ! modify.e1^
                                                                                                                        end

  val name = Field("name", "eric")
  val age = Field("age", 18)
  val ageError = Field("age", {sys.error("error"); 18})

  case object creation {
    def e1 = Field(18).label must_== ""
    def e2 = age() must_== 18 
    def e3 = {
      Field("person", name, age).toString must_== "person: eric/18"
    }
  }
  case object execute {
    def e1 = age.execute must_== skipped
    def e2 = ageError.execute must be like { case Error(_, _) => ok }
  }
  case object modify {
    def e1 = age.toStringField() must_== "18"
  }
}