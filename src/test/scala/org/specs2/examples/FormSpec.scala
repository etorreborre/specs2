package org.specs2
package examples
import specification._
import form._

class FormSpec extends SpecificationWithJUnit with Forms { def is =                 
                                                                                                                        """
This shows an example of Forms in a specification.
You can run this specification by executing `specs2.html org.specs2.examplesFormSpec`.

This will create a html file in the target/specs2-reports directory
                                                                                                                        """^
                                                                                                                        p^
 "A person object must have proper initials"                                                                            ^
   person("Eric", "Torreborre", initials = "E.T.")                                                                      ^
                                                                                                                        p^
 "The address should be as expected"                                                                                    ^
   Address("Oxford St", 12).
     fill("Oxford St", 12)                                                                                              ^
                                                                                                                        p^
 "A person can have 2 addresses"                                                                                        ^
   Form("Addresses").tr {
     tab("home",
       Address("Oxford St", 12).
       fill("Oxford St", 12)).
     tab("work",
       Address("Rose Cr.", 3).
       fill("Rose Cr.", 3))
   }                                                                                                                    ^
                                                                                                                        end
                                                                                          
  def person(first: String, last: String, initials: String) =  
     Form("Person").
       tr(field("first name", first)).
       tr(field("last name", last)).
       tr(prop("initials", Person(first, last).initials)(initials))

  case class Person(firstName: String, lastName: String) {
    def initials = firstName.take(1).capitalize+"."+lastName.take(1).capitalize+"."      
  }              


  case class Address(street: String, number: Int) {
    def form = fill(street, number)
    def fill(s: String, n: Int) =
      Form("Address").
          tr(prop("street", s)(street)).
          tr(prop("number", n)(number))
  }

}