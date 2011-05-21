package org.specs2
package form
import FormsBuilder._

class CellsSpec extends Specification { def is =
                                                                                                                        """
Cells are responsible for the formatting of Fields, Props and Form added to a Form.
                                                                                                                        """^
  "Field cells can format fields as text"                                                                               ^
    "to display the label and value of the Field"                                                                       !
      { fieldCell.text must_== "name: 3" }                                                                              ^
                                                                                                                        p^
  "Prop cells can format props as text"                                                                                 ^
    "to display the label and expected value of the Prop"                                                               !
    { propCell.text must_== "name: 3" }                                                                                 ^
                                                                                                                        end

  val fieldCell = FieldCell(field("name", 3))
  val propCell = PropCell(prop("name", 3)(3))

}