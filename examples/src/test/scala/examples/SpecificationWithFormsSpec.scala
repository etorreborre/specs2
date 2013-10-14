package examples

import org.specs2._
import specification._
import form._

/**
 * This specification show how to use Forms to display tables in html documents
 */
class SpecificationWithFormsSpec extends Specification with Forms { def is = s2"""

 This shows an example of Forms in a specification.
 You can run this specification by executing

 * `specs2.html org.specs2.examples.UseFormSpec` or
 * `sbt> test-only org.specs2.examples.UseFormSpec -- html`

 This will create a html file in the target/specs2-reports directory


 A person object must have proper initials
   ${ person("Eric", "Torreborre", initials = "E.T.") }

 The address should be as expected
   ${ Address("Oxford St", 12).
       fill("Oxford St", 12) }

 A person can have 2 addresses
   ${ Form("Addresses").tr {
       tab("home",
           Address("Oxford St", 12).
             fill("Oxford St", 12)).
        tab("work",
            Address("Rose Cr.", 3).
              fill("Rose Cr.", 3)) }
   }
                                                                                 """
                                                                                          
  def person(first: String, last: String, initials: String) =  
     Form("Person").
       tr(field("first name", first)).
       tr(field("last name", last)).
       tr(prop("initials", Person(first, last).initials)(initials))

  case class Person(firstName: String, lastName: String) {
    def initials = firstName.take(1).capitalize+"."+lastName.take(1).capitalize+"."      
  }              
}

import specification.Forms._

case class Address(street: String = "", number: Int = 0) {
  def address = number+", "+street
  def retrieve(i: Int) = this
  def actualIs(a: Address) = this
  def form = fill(street, number)
  def fill(s: String, n: Int) =
    Form("Address").
        tr(prop("street", s)(street)).
        tr(prop("number", n)(number))

}
