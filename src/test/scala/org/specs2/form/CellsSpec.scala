package org.specs2
package form
import Forms._

class CellsSpec extends SpecificationWithJUnit {
  def is = 
                                                                                          p^
" Text output for cells"                                                                  ^                                                                     
"   for FieldCell(name, 3)"         ! { fieldCell.text must_== "name: 3" }                ^   
"   padText(5) for FieldCell"       ! { fieldCell.padText(Some(5)) must_== "name: 3" }    ^   
"   padText(10) for FieldCell"      ! { fieldCell.padText(Some(10)) must_== "name: 3   " }^   
                                                                                          p^
"   for PropCell(prop(name, 5)(3))" ! { propCell.text must_== "name: 3" }                 ^   
"   padText(5) for PropCell"        ! { propCell.padText(Some(5)) must_== "name: 3" }     ^   
"   padText(10) for PropCell"       ! { propCell.padText(Some(10)) must_== "name: 3   " } ^  
                                                                                          end
  val fieldCell = FieldCell(field("name", 3))
  val propCell = PropCell(prop("name", 3)(3))

}