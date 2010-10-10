package org.specs2
package form
import Forms._
import specification._

class FormSpec extends SpecificationWithJUnit {
  val content = 
""""
  A Form is a generic table which has a title (optional) and rows.
  Each row contains cells which can be created from Fields, Props or other Forms.

  A Form is usually created in a specification with expected values so that it
  can be displayed with the rest of the text (whereas a DataTable is only displayed
  when there are failures. It is then "implemented" with actual values in an example. 
  Upon execution a Form will return a Result value summarizing the execution of each 
  Prop it embbeds.

"""                                                                            ^                        
" A Form can be created"                                                       ^
"   with a title"                                                              ! creation.e1 ^
"   with one field on one row"                                                 ! creation.e2 ^
"   with two fields on one row"                                                ! creation.e3 ^
"   with a title and on field on each row"                                     ! creation.e4 ^
"   with a property on one row"                                                ! creation.e5 ^
"   with another form on one row"                                              ! creation.e6 ^
                                                                               p^
" A Form can be displayed, showing expected values"                            ^
"   with its title"                                                            ^
"     if present: | title |"                                                   ! display.e1 ^
"     if absent: the text is empty"                                            ! display.e2 ^
"   with one row only"                                                         ^
"     and one cell"                                                            ! display.e3 ^
"     and 2 cells"                                                             ! display.e4 ^
"   with a title and one row"                                                  ^
"     and one cell"                                                            ! display.e5 ^
"     and 2 cells"                                                             ! display.e6 ^
                                                                               p^
" A Form can be added"                                                         ^
"   as a Fragment in a specification, to show all expected values"             ! fragments.e1^
"   to an example, returning success if the form is a success"                 ! fragments.e2 ^
"   to an example, returning a failure if one property in the  form fails"     ! fragments.e3 ^
                                                                               end
                                                                               
  object creation {
    def e1 = Form("title").title must_== Some("title")
    def e2 = Form.tr(field("name", "eric")).rows.size must_== 1
    def e3 = Form.tr(field("name", "eric"), field("age", 18)).rows.size must_== 1
    def e4 = Form.tr(field("name", "eric")).
                  tr(field("age", 18)).rows.size must_== 2
    def e5 = Form("title").tr(prop("name", "eric")).rows.size must_== 1
    def e6 = Form("title").tr(form("title")).rows.size must_== 1
  }    
  object display {
    val name = field("name", "eric")
    val age = field("age", 18)
 
    def e1 = form("title").text must_== "| title |"
    def e2 = Form().text must_== ""
    def e3 = Form.tr(name).text must_== "| name: eric |"
    def e4 = Form.tr(name, age).text must_== "| name: eric | age: 18 |"
    def e5 = form("title").tr(name).text must_==
             "| title |\n" +    
             "| name: eric |"
    def e6 = form("title").tr(name, age).text must_== 
             "| title |\n" + 
             "| name: eric | age: 18 |"
  }
  
  object fragments {
    def form = Form("Customer").tr(field("name", "eric"), field("age", 18))
    def e1 = ("This is the expected customer" ^ form).fragments.size must_== 2
    def e2 = {
      val example = "the customer must be as expected" ! form
      example.execute.isSuccess must beTrue
    }
    def e3 = {
      trait Customer {
        val name: String
        val age: Int
        def form = Form("Customer").tr(prop("name", "eric")(name), prop("age", 20)(age))
      }
      
      val example = "the customer must be as expected" ! new Customer {
        val name = "eric"
        val age = 18
      }.form
      example.execute.message must_== "'20' is not equal to '18'"
      
    }
  }
}
