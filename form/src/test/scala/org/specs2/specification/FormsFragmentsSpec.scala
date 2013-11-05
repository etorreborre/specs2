package org.specs2
package specification

import form._
import matcher._

class FormsFragmentsSpec extends Specification with Forms with ThrownExpectations { def is = s2"""
  
 A form can be added as a Fragment in a specification
   creating a new Text Fragment                                                      ${frags.e1_1}
   showing all expected values                                                       ${frags.e1_2}
   in an interpolated spec                                                           ${frags.e1_3}
   with an implicit `form` method                                                    ${frags.e1_4}

 It can also be added as the body of an example
   returning success if the form is a success                                        ${frags.e2}
   returning a failure if one property in the  form fails                            ${frags.e3}
                                                                                     """
                                                                               
  object frags extends Customers {
    def e1_1 = ("This is the expected customer" ^ form).middle.size must_== 2
    def e1_2 = ("This is the expected customer" ^ form).middle(1).text must_== s"Example($formText)"
    def e1_3 = s2"This is the expected customer $form".middle(2).text must_== s"Example($formText)"

    def e1_4 = {
      val spec = s2"This is the expected customer $eric"
      spec.middle(1).text must_== "Text(This is the expected customer )"
      spec.middle(2).text must_== s"Example($formText)"
    }

    def e2 = {
      val example = "the customer must be as expected" ! form
      example.execute.isSuccess must beTrue
    }
    def e3 = {
      val example = "the customer must be as expected" ! failedForm
      example.execute.message must_== "'20' is not equal to '18'"
      
    }
  }

  trait Customers {
    trait Customer {
      val name: String
      val age: Int
      def form = Form("Customer").
        tr(prop("name", "eric")(name), prop("age", 20)(age))
    }

    val eric = new Customer { val name = "eric"; val age = 20 }

    val formText =
     "| Customer             |\n"+
     "| name: eric | age: 20 |"

    def form = eric.form

    def failedForm =  new Customer {
      val name = "eric"
      val age = 18
    }.form
  }
}
