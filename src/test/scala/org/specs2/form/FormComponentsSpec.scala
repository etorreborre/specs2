package org.specs2
package form
import Forms._
import specification._

class FormComponentsSpec extends SpecificationWithJUnit { val content = 
"""
  Forms can be used to represent domain objects or service, relating
  expected values to actual ones. Forms are also thought as being reusable components which can
  be reused between specifications. In the following examples, we'll see several ways of
  creating forms to support the specification of domain objects or services:

  * the first example shows how to create a simple form component representity an Address entity
    * it is modeled as a trait declaring expected values
    * it has a form method which
      * has default parameters for the actual values of the form
      * creates the form by putting each property on a new row
    * the Address trait can be instantiated once with expected values, to be displayed as a Fragment
    * then the form method can be used in a example to provide actual values

  * the second example shows how to specify 2 domain objects, in a parent-child relationship:
    Customer-Address:
    * the Customer trait defines a name attribute and embbeds an instance of Address
    * the Customer Form is defined by putting the name on one row and the Address form on the second
      row

"""                                                                                                 ^                        
" A Form can be created for a simple Address object"                                                ^
"   it can be displayed with expected values"                                                       ^
      components.address.form()                                                                     ^
"   it can then be used in an example and executed with actual values"                              ! components.e1^
                                                                                                    p^
" A Form can be created for a Customer object with an Address"                                      ^
"   it can be displayed with expected values"                                                       ^
      components.customer.form()                                                               ^
"   it can then be used in an example and executed with actual values"                              ! components.e2^
                                                                                                    end
  object components {
    trait Address {
      val street: String
      val number: Int
      def form(s: String = street, n: Int = number) = 
        Form("Address").
            tr(prop("street", s)(street)).
            tr(prop("number", n)(number))
    }
    val address = new Address {
      val street = "Rose Crescent"
      val number = 2
    }
    trait Customer {
      val name: String
      val address: Address
      def form(na: String = name, a: Form = address.form()) =
        Form("Customer").
            tr(prop("name", na)(name)).
            tr(a)
           
    }
    val customer = new Customer {
      val name = "Eric"
      val address = new Address {
        val street = "Rose Crescent"
        val number = 2
      }
    }
    def e1 = address.form("Rose Crescent", 5).execute.message must_== "'5' is not equal to '2'" 
    def e2 = customer.form("Eric", 
                           customer.address.form("Rose Crescent", 5)).execute.message must_== "'5' is not equal to '2'" 
  }
}
