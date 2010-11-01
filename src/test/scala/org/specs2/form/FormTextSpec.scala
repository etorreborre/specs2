package org.specs2
package form
import FormsBuilder._

class FormTextSpec extends SpecificationWithJUnit { 
  def is = 
                                                                                          """
  Forms can be displayed as text in the ConsoleReporter for example.
  This specification shows what's expected for different kind of forms.
                                                                                          """^
" A simple form with 2 fields"+address1                                                   ^
"   must align each end of row"                                                           !e1^
                                                                                          p^
" A simple form with 3 fields, 2 on one row and one on the second row\n"+address2         ^
"   must align each end of row"                                                           !e2^  
                                                                                          p^
" A form with more fields and rows"+address3                                              ^
"   must align each end of row"                                                           !e3^  
                                                                                          p^
" A form with an embedded form"+address4                                                  ^
"   must align each end of row"                                                           !e4^  
                                                                                          end
  val street = field("street", "Rose Crescent")
  val number = field("number", 2)
  val town = field("town", "Mosman")
  
  val address = Form("Address").
                     tr(street).
                     tr(number)
                     
  val address1 =  quote(address.text)
                 
  val address2 = quote(Form("Address").
                       tr(street, number).
                       tr(town).text)
  
  val address3 = quote(Form.
                       tr(street, number).
                       tr(town, street, number).
                       tr(town).text)

  val address4 = quote(Form.
                       tr(town).
                       tr(address).text)

  def e1 = compare(address1, 
           "| Address               |",                                                                                            
           "| street: Rose Crescent |",                                                                                            
           "| number: 2             |")                                                                                            

  def e2 = compare(address2,
           "| Address                           |",                                                                                            
           "| street: Rose Crescent | number: 2 |",                                                                                            
           "| town: Mosman                      |")        
           
  def e3 = compare(address3,
           "| street: Rose Crescent | number: 2                         |",                                                                                            
           "| town: Mosman          | street: Rose Crescent | number: 2 |",                                                                                            
           "| town: Mosman                                              |")        

  def e4 = compare(address4,
           "| town: Mosman          |",                                                                                            
           "| Address               |",                                                                                            
           "| street: Rose Crescent |",                                                                                            
           "| number: 2             |")        

  def quote(s: String) = "\n"+s+"\n"           
  def compare(form: String, expected: String*) = form must_== expected.mkString("\n", "\n", "\n")           
}