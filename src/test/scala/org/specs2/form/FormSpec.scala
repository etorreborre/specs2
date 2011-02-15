package org.specs2
package form
import FormsBuilder._
import specification._

class FormSpec extends SpecificationWithJUnit { def is =
                                                                                                                        """
A Form is a generic table which has an optional title and rows.
Each row contains cells which can be created from Fields, Props or other Forms.

A Form is usually created in a specification with expected values so that it
can be displayed with the rest of the text (whereas a DataTable is only displayed
when there are failures. It is then "implemented" with actual values in an example.
Upon execution a Form will return a Result value summarizing the execution of each
Prop it embeds.
                                                                                                                        """^
                                                                                                                        p^
"A Form can be created"                                                                                                 ^
  "with a title"                                                                                                        ! creation.e1 ^
  "with one field on one row"                                                                                           ! creation.e2 ^
  "with two fields on one row"                                                                                          ! creation.e3 ^
  "with a title and one field on each row"                                                                              ! creation.e4 ^
  "with a property on one row"                                                                                          ! creation.e5 ^
  "with another Form on one row"                                                                                        ! creation.e6 ^
                                                                                                                        p^
"A Form can be displayed, showing expected values"                                                                      ^
  "with its title"                                                                                                      ^
    "if present: | title |"                                                                                             ! display.e1 ^
    "if absent: the text is empty"                                                                                      ! display.e2 ^
                                                                                                                        p^
  "the columns must be aligned for"                                                                                     ^
    "a simple form with 2 fields" + address1                                                                            ! align.e1 ^
    "a simple form with 3 fields, 2 on one row and one on the second row\n"+address2                                    ! align.e2 ^
    "a form with more fields and rows" + address3                                                                       ! align.e3 ^
    "a form with an inlined form" + address4                                                                            ! align.e4 ^
                                                                                                                        p^
  "with one row only"                                                                                                   ^
    "and one cell"                                                                                                      ! display.e3 ^
    "and 2 cells"                                                                                                       ! display.e4 ^
                                                                                                                        p^
  "with a title and one row"                                                                                            ^
    "and one cell"                                                                                                      ! display.e5 ^
    "and 2 cells"                                                                                                       ! display.e6 ^
                                                                                                                        endp^
"A Form can be executed as a success"                                                                                   ^
  "then its rows are a success"                                                                                         ! exec.e1^
  "and row cells are a success"                                                                                         ! exec.e2^
                                                                                                                        p^
"A Form can be executed as a failure"                                                                                   ^
  "then its rows are a failure"                                                                                         ! exec.e3^
  "and row cells are a failure"                                                                                         ! exec.e4^
                                                                                                                        p^
"Forms rows and cells have equals/hashcode methods"                                                                     ^
  "row1 == row1"                                                                                                        ! equality.e1^
  "cell1 == cell1"                                                                                                      ! equality.e2^
                                                                                                                        p^
"Forms can be displayed as xhtml"                                                                                       ^
  "the title must span all columns"                                                                                     ! xhtml.e1^
                                                                                                                        p^
"A form can be added to another"                                                                                        ^
    "inlined"                                                                                                           ! nested.e1^
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
             "| title      |\n" +    
             "| name: eric |"
    def e6 = form("title").tr(name, age).text must_==
             "| title                |\n" + 
             "| name: eric | age: 18 |"
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

  object xhtml {
    // count 3 per prop or field
    def e1 = Xml.colnumber(new FormCell(Form.th("title").
      tr(field(1)).
      tr(field("n", "v"), field("n", "v")).
      tr(prop("p", 1)(2)))) must_== 6
  }

  object nested {
    def e1 = Form("title").
             tr(Form.tr("hello").inline).toXml must ==/(
              <form>
                <table class="dataTable">
                  <tr><th colspan="4">title</th></tr><tr><div colspan="3"><tr><td style="" class="info" colspan="3">hello</td></tr></div></tr>
                </table>
              </form>)
  }

                                                                                                                          end
  val street = field("street", "Rose Crescent")
  val number = field("number", 2)
  val town = field("town", "Mosman")

  val address = Form("Address").
                     tr(street).
                     tr(number)

  val address1 =  quote(address.text)

  val address2 = quote(Form("Address").
                       tr(street, number).
                       tr(town).text)

  val address3 = quote(Form.
                       tr(street, number).
                       tr(town, street, number).
                       tr(town).text)

  val address4 = quote(Form.
                       tr(town).
                       tr(address.inline).text)

  object align {
    def e1 = compare(address1,
             "| Address               |",
             "| street: Rose Crescent |",
             "| number: 2             |")

    def e2 = compare(address2,
             "| Address                           |",
             "| street: Rose Crescent | number: 2 |",
             "| town: Mosman                      |")

    def e3 = compare(address3,
             "| street: Rose Crescent | number: 2                         |",
             "| town: Mosman          | street: Rose Crescent | number: 2 |",
             "| town: Mosman                                              |")

    def e4 = compare(address4,
             "| town: Mosman          |",
             "| Address               |",
             "| street: Rose Crescent |",
             "| number: 2             |")

    def compare(form: String, expected: String*) = form must_== expected.mkString("\n", "\n", "\n")
  }
  def quote(s: String) = "\n"+s+"\n"
}
