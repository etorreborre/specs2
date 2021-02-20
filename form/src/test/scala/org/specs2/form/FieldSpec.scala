package org.specs2
package form

import sys.*
import execute.*
import matcher.*

class FieldSpec extends Spec with TypedEqual { def is = s2"""

A Field is a labelled property with can be embedded in a Form.

 # A Field can be created
   from just a value (then its name is empty) $creation1
   from a name and a value                    $creation2
   from existing fields, concatenating them   $creation3

 # A Field can be executed
   it returns skipped if the value is ok                $execution1
   it returns an error if the value throws an exception $execution2

 # A Field can be modified
   to a string Field $modification1

"""

  val name     = Field("name", "eric")
  val age      = Field("age", 18)
  val ageError = Field("age", { error("error"); 18 })

  def creation1 = Field(18).label                     === ""
  def creation2 = age.toOption                        === Some(18)
  def creation3 = Field("person", name, age).toString === "person: eric/18"

  def execution1 = age.execute      `must` ===(skipped)
  def execution2 = ageError.execute `must` beLike { case Error(_, _) => ok }

  def modification1 = age.toStringField.toOption `must` ===(Some("18"))
}
