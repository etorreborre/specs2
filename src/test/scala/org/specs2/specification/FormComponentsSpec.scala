package org.specs2
package specification

class FormComponentsSpec extends SpecificationWithJUnit with Forms { def is = 
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
    * then the form method can be used in a example to provide actual values              """^
                                                                                          p^
    components.address.form                                                               ^
                                                                                          p^
"   The Address form can then be used in an example and executed with actual values"      ! components.e1^
                                                                                          end^
                                                                                          """                        

Second example: Aggregate entity
--------------------------------

  The second example shows how to specify 2 domain objects, in a parent-child
  relationship like Customer-Address:
    
    * the Customer case class defines a name attribute and embeds an instance of the 
      Address class
    * the Customer Form is defined by setting the name on one row and the Address form 
      on the second row                                                                   """^                        
                                                                                          p^
    components.customer.form                                                              ^
                                                                                          p^
"   The Customer (and Address component) can then be used in an example"                  +
"   and automatically executed with actual values"                                        ! components.e2^
                                                                                          end^
                                                                                          """                        

Third example: Decision table
-----------------------------

  The third example shows how to specify a table with columns where some columns must be
  the result of a computation involving other columns:
  
    * the initials column specifies the expected values for a given first name and last name
      "eric" "torreborre" => "E.T."
                                                                                          """^                        
    components.initialsTable.form.setSuccess                                              ^
                                                                                          p^
" The table can then be used in an example and executed with actual values"               ! components.e3^
                                                                                          end^
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
                                                                                          """^                        
    components.order.form                                                                 ^
                                                                                         p^
"It is possible to check if expected rows are a subset of actual rows"                   ^
  "if the expected rows are contained in the actual rows, it succeeds"                   ! components.e4^
  "if the expected rows are not contained in the actual rows, it fails"                  ! components.e5^
                                                                                         p^
"It is possible to check if expected rows are a subsequence of actual rows,"             +
"(in the same order)"                                                                    ^
  "if the expected rows are in the same order, it succeeds"                              ! components.e6^
  "if the expected rows are in a different order, it fails"                              ! components.e7^
                                                                                         p^
"It is possible to check if expected rows are the actual rows, in any order"             ^
  "if the expected rows are the same, it succeeds"                                       ! components.e8^
  "if the expected rows are not same, it fails"                                          ! components.e9^
                                                                                         p^
"It is possible to check if expected rows are the actual rows,"                          +
"(in the same order)"                                                                    ^
  "if the expected rows are the same, in the same order, it succeeds"                    ! components.e10^
  "if the expected rows are the same, in an other order, it fails"                       ! components.e11^
                                                                                         end
  object components extends ComponentsDefinitions {
    val address = Address(street = "Rose Crescent", number = 2)
    val customer = Customer(name = "Eric", address = Address(street = "Rose Crescent", number = 2))
    
    val initialsTable = initials().
      tr("eric",  "torreborre", "E.T.").
      tr("hello", "world",      "H.Wo.")  
   
    val order = Order().
      line(OrderLine("PIS", 1)).            
      line(OrderLine("PS", 2)).
      line(OrderLine("Beginning Scala", 3))
    
    def e1 = address.fill("Rose Crescent", 5).execute.message must_== "'5' is not equal to '2'" 
    def e2 = customer.fill("Eric", 
                           customer.address.fill("Rose Crescent", 5)).execute.message must_== "'5' is not equal to '2'"
                             
    def e3 = initialsTable.form.execute.message must_== "'H.W.' is not equal to 'H.Wo.'"
    def e4 = {
      order.fillSubset(
        OrderLine("PIS", 1),
        OrderLine("PS", 2)
      ).execute must_== success
    }
    def e5 = {
      order.fillSubset( 
        OrderLine("PS", 2),
        OrderLine("BS", 3)
      ).execute.isSuccess must beFalse
    }
    def e6 = order.fillSubsequence(
        OrderLine("PS", 2),
        OrderLine("Beginning Scala", 3)
      ).execute must_== success

    def e7 = order.fillSubsequence(
        OrderLine("Beginning Scala", 3),
        OrderLine("PIS", 1),
        OrderLine("PS", 2)
      ).execute.isSuccess must beFalse
      
    def e8 = order.fillSet(
        OrderLine("Beginning Scala", 3),
        OrderLine("PS", 2),
        OrderLine("PIS", 1)
      ).execute.isSuccess must beTrue
      
    def e9 = order.fillSet(
        OrderLine("Beginning Scala", 3),
        OrderLine("PS", 2)
      ).execute.isSuccess must beFalse
      
    def e10 = order.fillSequence(
        OrderLine("Beginning Scala", 3),
        OrderLine("PIS", 1)
      ).execute.isSuccess must beFalse
      
    def e11 = order.fillSequence(
        OrderLine("Beginning Scala", 3),
        OrderLine("PS", 2)
      ).execute.isSuccess must beFalse

  }
}
