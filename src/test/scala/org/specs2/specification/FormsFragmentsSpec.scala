package org.specs2
package specification
import form._

class FormsFragmentsSpec extends SpecificationWithJUnit with Forms { def is = 
  
  "A form can be added as a Fragment in a specification"                                 ^
    "creating a new Text Fragment"                                                       ! fragments.e1_1^
    "showing all expected values"                                                        ! fragments.e1_2^
                                                                                         p^
  "It can also be added as the body of an example"                                       ^
    "returning success if the form is a success"                                         ! fragments.e2 ^
    "returning a failure if one property in the  form fails"                             ! fragments.e3 ^
                                                                                         end
                                                                               
  object fragments {
    trait Customer {
      val name: String
      val age: Int
      def form = Form("Customer").
                 tr(prop("name", "eric")(name), prop("age", 20)(age))
    }
    def form =  new Customer {
      val name = "eric"
      val age = 20
    }.form
    
    def failedForm =  new Customer {
      val name = "eric"
      val age = 18
    }.form
    
    def e1_1 = ("This is the expected customer" ^ form).fragments.size must_== 2
    def e1_2 = ("This is the expected customer" ^ form).fragments(1) must_== Example( 
                "| Customer             |\n"+
                "| name: eric | age: 20 |", success)
    def e2 = {
      val example = "the customer must be as expected" ! form
      example.execute.isSuccess must beTrue
    }
    def e3 = {
      val example = "the customer must be as expected" ! failedForm
      example.execute.message must_== "'20' is not equal to '18'"
      
    }
  }
  
  object exec {
    def e1 = Form.tr("a").setSuccess.execute must_== success
    def e2 = Form.tr("a").setSuccess.rows.forall(_.execute.isSuccess) must_== true
    def e3 = Form.tr("a").setFailure.execute.message must_== failure.message
    def e4 = Form.tr("a").setFailure.rows.forall(_.execute.isSuccess) must_== false
  }
  
  object equality {
    def e1 = Row.tr(TextCell("a")) must_== Row.tr(TextCell("a"))   
    def e2 = TextCell("a") must_== TextCell("a")   
 }
}
