package org.specs2
package form
import FormsBuilder._

class CellsSpec extends SpecificationWithJUnit { def is = 
                                                                                                                        """
Cells are responsible for the formatting of Fields, Props and Form added to a Form.
                                                                                                                        """^
                                                                                                                        p^
"Field cells can format fields as text"                                                                                 ^
  "to display the label and value of the Field"                                                                         !
    { fieldCell.text must_== "name: 3" }                                                                                ^
  "to display the text, right padding with a number of spaces"                                                          ^
    "changing nothing if the padding numberis < to the text size"                                                       !
    { fieldCell.padText(Some(5)) must_== "name: 3" }                                                                    ^
    "adding spaces if the padding number is > to the text size"                                                         !
    { fieldCell.padText(Some(10)) must_== "name: 3   " }                                                                ^
                                                                                                                        p^
"Prop cells can format props as text"                                                                                   ^
  "to display the label and expected value of the Prop"                                                                 !
    { propCell.text must_== "name: 3" }                                                                                 ^
  "to display the text, right padding with a number of spaces"                                                          ^
    "changing nothing if the padding numberis < to the text size"                                                       !
    { propCell.padText(Some(5)) must_== "name: 3" }                                                                     ^
    "adding spaces if the padding number is > to the text size"                                                         !
    { propCell.padText(Some(10)) must_== "name: 3   " }                                                                 ^
                                                                                                                        end

  val fieldCell = FieldCell(field("name", 3))
  val propCell = PropCell(prop("name", 3)(3))

}