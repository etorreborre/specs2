package org.specs2
package form
import Forms._
class FormSpec extends SpecificationWithJUnit {
  val content = 
""""
  A Form is a generic table which generally has a title and rows.
  Each row is a list of either Fields, Props or nested Forms.
"""                                                                            ^                        
" A Form can be created"                                                       ^
"   with a title"                                                              ! creation.e1 ^
"   with one field on one row"                                                 ! creation.e2 ^
"   with two fields on one row"                                                ! creation.e3 ^
"   with a title and on field on each row"                                     ! creation.e4 ^
"   with a property on one row"                                                ! creation.e5 ^
"   with another form on one row"                                              ! creation.e6 ^
                                                                               p^
" A Form can be displayed"                                                     ^
"   with its title"                                                            ^
"     if present: | title |"                                                   ! display.e1 ^
"     if absent: the text is empty"                                            ! display.e2 ^
"   with one row only"                                                         ^
"     and one cell"                                                            ! display.e3 ^
"     and 2 cells"                                                             ! display.e4 ^
"   with a title and one row"                                                  ^
"     and one cell"                                                            ! display.e5 ^
"     and 2 cells"                                                             ! display.e6 ^
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
    def e1 = form("title").text must_== "| title |"
    def e2 = Form().text must_== ""
    def e3 = Form.tr(field("name", "eric")).text must_== "| name: eric |"
    def e4 = Form.tr(field("name", "eric"), field("age", 18)).text must_== "| name: eric | age: 18 |"
    def e5 = form("title").tr(field("name", "eric")).text must_==
             "| title      |\n" +    
             "| name: eric |"
    def e6 = form("title").tr(field("name", "eric"), field("age", 18)).text must_== 
             "| title                |\n" + 
             "| name: eric | age: 18 |"
  }                                                                                           
}
