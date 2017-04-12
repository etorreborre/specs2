package org.specs2
package specification

import form._
import matcher._
import org.specs2.specification.core.SpecStructure
import org.specs2.specification.process.DefaultExecutor

class FormsFragmentsSpec extends Specification with Forms with ThrownExpectations { def is = s2"""
  
 A form can be added as a Fragment in a specification
   creating a new Text Fragment                                                      ${frags.e1_1}
   showing all expected values                                                       ${frags.e1_2}
   in an interpolated spec                                                           ${frags.e1_3}
   with an implicit `form` method                                                    ${frags.e1_4}

 It can also be added as the body of an example
   returning success if the form is a success                                        ${frags.e2}
   returning a failure if one property in the form fails                             ${frags.e3}
                                                                                     """
                                                                               
  object frags extends Customers {
    def e1_1 = execute("This is the expected customer" ^ form).size must_== 2
    def e1_2 = execute("This is the expected customer" ^ form).map(_.description.show).apply(1) must_== s"$formText"
    def e1_3 = execute(s2"This is the expected customer $form").map(_.description.show).apply(1) must_== s"$formText"

    def e1_4 = {
      val spec = execute(s2"This is the expected customer $eric").map(_.description.show)
      spec(0).toString must_== "This is the expected customer "
      spec(1).toString must_== s"$formText"
    }

    def e2 = {
      val example = "the customer must be as expected" ! form
      example.executionResult.isSuccess must beTrue
    }
    def e3 = {
      val example = DefaultExecutor.execute("the customer must be as expected" ! failedForm)
      example.executionResult.message must_== "20 != 18"
      
    }

    def execute(spec: SpecStructure) =
      DefaultExecutor.executeAll(spec.fragments.fragments:_*)
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
