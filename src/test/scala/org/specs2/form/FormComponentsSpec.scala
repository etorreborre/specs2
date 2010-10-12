package org.specs2
package form
import Forms._
import specification._

class FormComponentsSpec extends SpecificationWithJUnit { val content = 
"""
  Forms can be used to represent domain objects or service, relating
  expected values to actual ones. Forms are also thought as being reusable components 
  which can be reused between specifications. In the following examples, we'll see several 
  ways of creating forms to support the specification of domain objects or services:
    
    1. for a simple Address entity 
    2. for a composite Customer entity and its Address instance
    3. for a decision table having some related columns
    4. for a composite Order - OrderLine entity (1-n) relationship

First example: Address entity
-----------------------------

  The first example shows how to create a simple form component representity an
    Address entity:
    
    * it is modeled as a case class declaring expected values
    * it has a form method which
      * has default parameters for the actual values of the form
      * creates the form by putting each property on a new row
    * the Address class can be instantiated once with expected values, to be displayed as
      a Fragment
    * then the form method can be used in a example to provide actual values
"""                                                                                       ^                        
    components.address.form                                                               ^
                                                                                          p^
"   The Address form can then be used in an example and executed with actual values"      ! components.e1^
                                                                                          p^
"""

Second example: Aggregate entity
--------------------------------

  The second example shows how to specify 2 domain objects, in a parent-child
  relationship like Customer-Address:
    
    * the Customer case class defines a name attribute and embeds an instance of the 
      Address class
    * the Customer Form is defined by setting the name on one row and the Address form 
      on the second row
"""                                                                                       ^
    components.customer.form                                                              ^
                                                                                          p^
"   The Customer (and Address component) can then be used in an example"                  +
"   and automatically executed with actual values"                                        ! components.e2^
                                                                                          p^
"""

Third example: Decision table
-----------------------------

  The third example shows how to specify a table with columns where some columns must be
  the result of a computation involving other columns:
  
    * the initials column specifies the expected values for a given first name and last name
      "eric" "torreborre" => "E.T."
"""                                                                                       ^
    components.initialsTable.form                                                         ^
                                                                                          p^
" The table can then be used in an example and executed with actual values"               ! components.e3^
                                                                                          p^
"""

Fourth example: 1-n relationship
--------------------------------

  The third example shows how to specify an entity, Order, which has several aggregated
  entities, OrderLines.
  In that case there are several things that we might want to specify:
  
    * the expected rows are included in the actual rows, with no specific order
      (this is the default case)
    * the expected rows are included in the actual rows, in the same order
    * the expected rows are exactly the actual rows, with no specific order
    * the expected rows are exactly the actual rows, in the same order
"""                                                                                       ^
    components.order.form                                                                 ^
                                                                                          p^
" The table can then be used in an example and executed with actual values"               ^
"   if the expected rows are contained in the actual rows, it succeeds"                   ! components.e4^
"   if the expected rows are not contained in the actual rows, it fails"                  ! components.e5^
                                                                                          end
  object components extends ComponentsDefinitions {
    val address = Address(street = "Rose Crescent", number = 2)
    val customer = Customer(name = "Eric", address = Address(street = "Rose Crescent", number = 2))
    
    val initialsTable = initials().
      tr("eric",  "torreborre", "E.T.").
      tr("hello", "world",      "H.Wo.")  
   
    val order = Order().
      line(OrderLine("PIS", 1)).            
      line(OrderLine("Beginning Scala", 3))
    
    def e1 = address.fill("Rose Crescent", 5).execute.message must_== "'5' is not equal to '2'" 
    def e2 = customer.fill("Eric", 
                           customer.address.fill("Rose Crescent", 5)).execute.message must_== "'5' is not equal to '2'"
                             
    def e3 = initialsTable.form.execute.message must_== "'H.W.' is not equal to 'H.Wo.'"
    def e4 = {
      order.fill(
        OrderLine("PIS", 1),
        OrderLine("Beginning Scala", 3),
        OrderLine("PS", 2)
      ).execute must_== success
    }
    def e5 = {
      order.fill( 
        OrderLine("PS", 2),
        OrderLine("Beginning Scala", 3)
      ).execute.isSuccess must beFalse
    }
  }
}
trait ComponentsDefinitions {
  case class Address(street: String, number: Int) {
    def form = fill(street, number)
    def fill(s: String, n: Int) = 
      Form("Address").
          tr(prop("street", s)(street)).
          tr(prop("number", n)(number))
  }
  case class Customer(name: String, address: Address) {
    def form = fill(name, address.form) 
    def fill(na: String, a: Form) =
      Form("Customer").
          tr(prop("name", na)(name)).
          tr(a)
         
  }
  case class initials(form: Form = Form.tr("First name", "Last name", "Initials")) {
    def computeInitials(f: String, l: String) = f(0).toUpper+"."+l(0).toUpper+"."
    
    def tr(firstName: String, lastName: String, expected: String) = initials {
      form.tr(firstName, lastName, prop(computeInitials(firstName, lastName))(expected))
    }
  }
  
  case class Order(lines: List[OrderLine] = Nil) {
    def line(orderLine: OrderLine) = Order(lines :+ orderLine)
    def form = fill(lines:_*)
    def fill(ls: OrderLine*) = Forms.form("Order", subset(lines, ls.toList))
  }
  case class OrderLine(name: String, quantity: Int) {
    def form = Form.tr(field("name", name), field("qty", quantity))
  }
}